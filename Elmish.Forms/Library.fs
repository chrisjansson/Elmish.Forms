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
                | Field.List ld -> Map.ofList [ ls.Id, value ]
                | _ -> failwith "should serialize to gd"
            | SchemaField.Group _
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
   
    let setField (id: FieldId) (value: FieldState) (model: Model) =
        let pathParts = Path.parse id
            
        let rec setRecursive (pathParts: Path list) (fields: Field) =
            match pathParts with
            | [] -> failwithf "Invalid path %s" id
            | [ Path.Node id ] ->
                match fields with
                | Field.Group g ->
                    g
                    |> Map.find id
                    |> (fun _ -> Map.add id (Field.Leaf value) g)
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
                    
        let (Field.Group newFormFields) = setRecursive pathParts (Field.Group model.FormFields)
        { model with FormFields = newFormFields }
            
    let addListItem (fullPath: FieldId) (model: Model) =
        let path = Path.parse fullPath
        let schema = Schema.getSchemaFromPath fullPath model
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
                | Path.List (s, index) ->
                    match fields with
                    | Field.List l ->
                        let modify node =
                            match setRecursive tail (Field.Group node) with
                            | Field.Group g -> g
                            | _ -> failwith "Must return group"
                        List.modifyI modify index l
                        |> Field.List
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
        
    let removeListItem (fullPath: FieldId) (index: int) (model: Model) =
        let path = Path.parse fullPath
        
        let rec setRecursive (pathParts: Path list) (fields: Field) =
            match pathParts with
            | [] ->
                match fields with
                | Field.List l ->
                    l
                    |> List.indexed
                    |> List.filter (fun (i, e) -> i <> index)
                    |> List.map (fun (_, e) -> e)
                    |> Field.List
                | _ -> failwithf "Expected to find list %A" fullPath
            | head::tail ->
                match head with
                | Path.List (s, index) ->
                    match fields with
                    | Field.List l ->
                        let modify node =
                            match setRecursive tail (Field.Group node) with
                            | Field.Group g -> g
                            | _ -> failwith "Must return group"
                        List.modifyI modify index l
                        |> Field.List
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
