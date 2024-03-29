﻿namespace Elmish.Forms

module Form =
    open Elmish.Forms.Types

    let initWithDefault (validator: Validator<_, _, 'Env>) (env: 'Env): Model =
        let data = 
            match validator.Schema with
            | SchemaField.Leaf ld ->
                let value = validator.Serialize env validator.InitFrom None
                Map.ofSeq [ ld.Id, value ]
            | SchemaField.List ls ->
                let value = validator.Serialize env validator.InitFrom None
                match value with
                | Field.List _ -> Map.ofList [ ls.Id, value ]
                | _ -> failwith "should serialize to gd"
            | SchemaField.Group _
            | SchemaField.Sub _
            | SchemaField.Type _ ->
                let value = validator.Serialize env validator.InitFrom None
                match value with
                | Field.Group gd -> gd
                | _ -> failwith "should serialize to gd"
        {
            FormFields = data
            Schema = validator.Schema
        }
        
    let init (validator: Validator<_, _, _>): Model =
        let formFields =
            match Schema.getDefaultForSchema validator.Schema with
            | Field.Group g -> g
            | Field.Leaf l ->
                let id = Schema.getId validator.Schema
                Map.ofList [ id, Field.Leaf l ]
            | _ -> failwith "Expects group when default initializing"
        
        {
            FormFields = formFields
            Schema = validator.Schema
        }
   
    let updateField (id: FieldId) (updater: FieldState -> FieldState) (model: Model) =
        let pathParts = Path.parse id
        
        let replaceLeaf (field: Field) =
            match field with
            | Field.Leaf l -> updater l |> Field.Leaf
            | _ -> failwith "expected leaf"
        
        let rec setRecursive (pathParts: Path list) (fields: Field) =
            match pathParts with
            | [] -> failwithf "Invalid path %s" id
            | [ Path.Node id ] ->
                match fields with
                | Field.Group g ->
                    g
                    |> Map.find id
                    |> (fun field -> Map.add id (replaceLeaf field) g)
                    |> Field.Group
                | _ -> failwithf "Invalid path %s" id
            | (Path.Node head)::tail ->
                match fields with
                | Field.Group g ->
                    g
                    |> Map.find head
                    |> (fun f -> Map.add head (setRecursive tail f) g)
                    |> Field.Group
                | _ -> failwithf "Invalid path %s, %A" id fields
            | (Path.List (head, index))::tail ->
                let updateList (field: Field) =
                    match field with
                    | Field.List l ->
                        let modify node =
                            match setRecursive tail (Field.Group node) with
                            | Field.Group g -> g
                            | _ -> failwith "Must return group"
                        List.modifyI modify index l
                        |> Field.List
                    | _ -> failwithf "Invalid path %s, %A" id fields
                match fields with
                | Field.Group g ->
                    g
                    |> Map.find head
                    |> (fun f -> Map.add head (updateList f) g)
                    |> Field.Group
                | _ -> failwithf "Invalid path %s, %A" id fields
                    
        match setRecursive pathParts (Field.Group model.FormFields) with
        | Field.Group newFormFields -> { model with FormFields = newFormFields }
        | _ -> failwith "Expected field group"
        
    let setField (id: FieldId) (value: string) (model: Model) =
        let updateFieldState (field: FieldState) =
            match field with
            | FieldState.String (_, data) -> FieldState.String (value, data)
        updateField id updateFieldState model 

    let setTouched (id: FieldId) (model: Model) =
        let updateFieldState (field: FieldState) =
            match field with
            | FieldState.String (s, data) -> FieldState.String (s, { data with IsTouched = true })
        updateField id updateFieldState model 
    
    let rec private getRecursive (id: FieldId) (fields: Field) =
        let pathParts = Path.parse id
        
        let rec getRecursive (pathParts) (fields: Field) =
            match pathParts with
            | [] -> failwithf "Invalid path %s" id
            | [ Path.Node id ] ->
                match fields with
                | Field.Group g ->
                    g
                    |> Map.find id
                | _ -> failwithf "Invalid path %s" id
            | (Path.Node head)::tail ->
                match fields with
                | Field.Group g ->
                    g
                    |> Map.find head
                    |> getRecursive tail
                | _ -> failwithf "Invalid path %s, %A" id fields
            | (Path.List (head, index))::tail ->
                let getInList (field: Field) =
                    match field with
                    | Field.List l ->
                        l.[index]
                        |> Field.Group
                        |> getRecursive tail
                    | _ -> failwithf "Invalid path %s, %A" id fields
                match fields with
                | Field.Group g ->
                    g
                    |> Map.find head
                    |> getInList
                | _ -> failwithf "Invalid path %s, %A" id fields
        getRecursive pathParts fields
    
    let getField (id: FieldId) (model: Model) =
        match getRecursive id (Field.Group model.FormFields) with
        | Field.Leaf l -> l
        | _ -> failwith "Path did not end at a leaf node"
    
    let addListItem (fullPath: FieldId) (model: Model) =
        let path = Path.parse fullPath
        let schema =
            match Schema.getSchemaFromPath fullPath model with
            | SchemaField.List l -> l.SubSchema
            | x -> x 
        let defaultAtPath =
            match Schema.getDefaultForSchema schema with
            | Field.Group g -> g
            | Field.Leaf l ->
                Map.ofList [
                    Schema.getId schema, Field.Leaf l
                ]
            | x -> failwithf "expected group from default %A" x
        
        let rec setRecursive (pathParts: Path list) (fields: Field) =
            match pathParts with
            | [] ->
                match fields with
                | Field.List l ->
                    List.append l [ defaultAtPath ]
                    |> Field.List
                | _ -> failwithf "Expected to find list %A" fullPath
            | head::tail ->
                match head with
                | Path.List (listName, index) ->
                    match fields with
                    | Field.List l ->
                        let modify node =
                            match setRecursive tail (Field.Group node) with
                            | Field.Group g -> g
                            | _ -> failwith "Must return group"
                        List.modifyI modify index l
                        |> Field.List
                    | Field.Group g ->
                        g
                        |> Map.find listName
                        |> (fun f -> Map.add listName (setRecursive pathParts f) g)
                        |> Field.Group
                    | _ -> failwithf "Invalid path %s, %A" fullPath fields
                | Path.Node head -> 
                    match fields with
                    | Field.Group g ->
                        g
                        |> Map.find head
                        |> (fun f -> Map.add head (setRecursive tail f) g)
                        |> Field.Group
                    | _ -> failwithf "Invalid path %s, %A" fullPath fields
        let field = setRecursive path (Field.Group model.FormFields)

        let fields =
            match field with
            | Field.Group g -> g
            | _ -> failwith "expected group from set"
    
        { model with FormFields = fields }
        
    let getListLength (fullPath: FieldId) (model: Model) =
        let field = getRecursive fullPath (Field.Group model.FormFields)
        match field with
        | Field.List l -> l.Length
        | _ -> failwithf "%s did not end in a list" fullPath 
        
    let removeListItem (fullPath: FieldId) (index: int) (model: Model) =
        let path = Path.parse fullPath
        
        let rec setRecursive (pathParts: Path list) (fields: Field) =
            match pathParts with
            | [] ->
                match fields with
                | Field.List l ->
                    l
                    |> List.indexed
                    |> List.filter (fun (i, _) -> i <> index)
                    |> List.map (fun (_, e) -> e)
                    |> Field.List
                | _ -> failwithf "Expected to find list %A" fullPath
            | head::tail ->
                match head with
                | Path.List (listName, index) ->
                    match fields with
                    | Field.List l ->
                        let modify node =
                            match setRecursive tail (Field.Group node) with
                            | Field.Group g -> g
                            | _ -> failwith "Must return group"
                        List.modifyI modify index l
                        |> Field.List
                    | Field.Group g ->
                        g
                        |> Map.find listName
                        |> (fun f -> Map.add listName (setRecursive pathParts f) g)
                        |> Field.Group
                    | _ -> failwithf "Invalid path %s, %A" fullPath fields
                | Path.Node head -> 
                    match fields with
                    | Field.Group g ->
                        g
                        |> Map.find head
                        |> (fun f -> Map.add head (setRecursive tail f) g)
                        |> Field.Group
                    | _ -> failwithf "Invalid path %s, %A" fullPath fields
        let field = setRecursive path (Field.Group model.FormFields)

        let fields =
            match field with
            | Field.Group g -> g
            | _ -> failwith "expected group from set"
    
        { model with FormFields = fields }
        
    let validate (validator: Validator<'r, 'env, _>) (env: 'env) (formFields: FormFields): ValidationResult<'r> =
        validator.Validate formFields { Env = env; Schema = validator.Schema } 
