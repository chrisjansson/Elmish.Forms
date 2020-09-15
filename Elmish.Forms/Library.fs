namespace Elmish.Forms

open System.Text.RegularExpressions


module Validator =
    open Elmish.Forms.Types

    let from (f: 'T): Validator<'T, 'Env, 'InitFrom> =
        let validate _ (_: Context<'Env>) =
            Ok f
        {
            Validate = validate
            Schema = SchemaField.Type { Type = sprintf "Custom validator form %s" (typeof<'T>.Name); Label = None; Fields = Map.empty }
            InitFrom = None
            Serialize = fun _ _ _ -> Field.Group (Map.empty)
        }
        
    let fromNamed (name: string) (f: 'T): Validator<'T, 'Env, 'InitFrom> =
        let validate _ (_: Context<'Env>) =
            Ok f
        {
            Validate = validate
            Schema = SchemaField.Type { Type = name; Label = None; Fields = Map.empty }
            InitFrom = None
            Serialize = fun _ _ _ -> Field.Group (Map.empty)
        }
    
    let apply (vf: Validator<_, _, _>) (va: Validator<_, _, _>): Validator<_, _, _> =
        let validate formFields (context: Context<_>) =
            let vfc = { Env = context.Env; Schema = vf.Schema }
            let vac = { Env = context.Env; Schema = va.Schema }
            
            match (vf.Validate formFields vfc, va.Validate formFields vac) with
            | Ok rf, Ok ra -> Ok (rf ra)
            | Ok _, Error e -> Error e
            | Error e, Ok _ -> Error e
            | Error l, Error r -> Error <| l@r
            
        let hydrate = fun v -> Some v
            
        let joinGroups g1 g2=
            match (g1, g2) with
            | Field.Group g, Field.Leaf frs ->
                let rightId = Schema.getId va.Schema

                let map = Map.add rightId (Field.Leaf frs) g
                Field.Group map
            | Field.Leaf fls, Field.Leaf frs ->
                let leftId = Schema.getId vf.Schema
                let rightId = Schema.getId va.Schema
                
                let map: FieldGroup = Map.ofSeq [
                    leftId, Field.Leaf fls
                    rightId, Field.Leaf frs
                ]
                
                Field.Group map
            | Field.Group gl, Field.Group gr ->
                let l = Map.toList gl
                let r = Map.toList gr
                Map.ofList (l @ r)
                |> Field.Group
            | _ -> failwithf "Unknown apply combination %A - %A" g1 g2
            
        let serialize env (initSelector: InitSelector<_, _> option) value =
            let hv1 =
                match vf.InitFrom with
                | Some hf1 -> (hf1 env) |> vf.Serialize env None
                | None -> vf.Serialize env None None
                
            let hv2 =
                match va.InitFrom with
                | Some hf1 -> (hf1 env) |> va.Serialize env None
                | None -> va.Serialize env None None
            joinGroups hv1 hv2

        let leftSchema =
            match vf.Schema with
            | SchemaField.Type t -> t
            | _ -> failwith "Left validator schema should always be Type"
        
        let rightSchema = 
            match va.Schema with
            | SchemaField.Leaf l -> [ l.Id, SchemaField.Leaf l ]
            | SchemaField.Sub s -> [ s.Id, SchemaField.Sub s ]
            | SchemaField.Group g -> g.Fields |> Map.toList
            | SchemaField.Type t -> Map.toList t.Fields
            
        let schema = SchemaField.Type
                         {
                             Label = leftSchema.Label
                             Type = leftSchema.Type
                             Fields = (leftSchema.Fields |> Map.toList) @ rightSchema |> Map.ofList
                         }
        
        {
            Validate = validate
            InitFrom = None
            Serialize = serialize
            Schema = schema
        }

    let withSub (subId: FieldId) (validator: Validator<_, _, _>): Validator<_, _, _> =
        
        let schema = SchemaField.Sub { Id = subId; SubSchema = validator.Schema }
        
        let serialize env (initSelector: InitSelector<_, _> option) (value: _ option) =
            let defaultNode = validator.Serialize env initSelector value
            
            Map.ofList [ subId, defaultNode ]
            |> Field.Group
        
        let validate: Validate<_, _> =
            fun formFields context ->
                let field =
                    match Map.find subId formFields with
                    | Field.Group g -> g
                    | _ -> failwith "Invalid field for validation"
                    
                match validator.Validate field context with
                | Ok r -> Ok r
                | Error e ->
                    let prepend (fieldId, message) = subId + "." + fieldId, message
                    let errors = List.map prepend e
                    Error errors
                
        {
          Validate = validate
          Schema = schema
          InitFrom = validator.InitFrom
          Serialize = serialize
        }
        
    let withList (listId: FieldId) (validator: Validator<'a, _, _>): Validator<'a list, _, _> =
        let serialize env (initSelector: InitSelector<_, _> option) (value: _ option) =
            let defaultNode = Field.List []
            match initSelector with
            | Some initSelector ->
                    match initSelector env with
                    | Some initValue ->
                        let unpackField  (field: Field) =
                            match field with
                            | Field.Group gd -> gd
                            | Field.Leaf ld ->
                                Map.ofList [ Schema.getId validator.Schema, Field.Leaf ld ]
                            | _ -> failwithf "Unsupported nesting %A" field
                        
                        initValue
                        |> List.map (fun value -> validator.Serialize value validator.InitFrom None)
                        |> List.map unpackField
                        |> Field.List
                        
                    | None ->
                        defaultNode
                
            | None ->
                defaultNode      
        
        let validate: Validate<_, _> =
            fun formFields context ->
                let field =
                    match Map.find listId formFields with
                    | Field.List l -> l
                    | _ -> failwith "Invalid field for validation"

                let runValidationForIndex index fields =
                    match validator.Validate fields context with
                    | Ok r -> Ok r
                    | Error e -> 
                        let prepend (fieldId, message) = listId + "["+ string index + "]." + fieldId, message
                        let errors = List.map prepend e
                        Error errors
                        
                field
                |> List.mapi (fun index f -> runValidationForIndex index f) //TODO: map context appropriately, a test that checks that isRequired can label correctly should work
                |> Result.traverse    
        
        {
          Validate = validate
          Schema = SchemaField.List { Id = listId; SubSchema = validator.Schema }
          InitFrom = None
          Serialize = serialize
        }
        
module Validators =
    open Elmish.Forms.Types

    let text (id: FieldId): Validator<string option, _, _> =
        let validate: Validate<string option, _> =
            fun (formFields: FormFields) _ ->
                match Map.tryFind id formFields with
                | Some field ->
                    match field with
                    | Field.Leaf fieldState ->
                        match fieldState with
                        | FieldState.String s ->
                            let result = 
                                if System.String.IsNullOrWhiteSpace(s) then
                                    None
                                else
                                    Some (s.Trim())
                            
                            Ok result
                    | _ -> failwith "Not implemented" //Error [ id, [] ] //Wrong type error //TODO: Test this case
                | None -> failwith "Not implemented" //Error [ id, [] ] //Field Id not found error //TODO: Test this case
                     
        let serialize env (initSelector: InitSelector<_, string option> option) (value: string option option) =
            let defaultNode = FieldState.String "" |> Field.Leaf
            
            let valueToSerialize =
                match initSelector with
                | Some initSelector ->
                    let selected = initSelector env
                    match selected with
                    | Some value -> value
                    | None -> None
                | None ->
                    match value with
                    | Some v -> v
                    | None -> None
                    
            match valueToSerialize with
            | Some s -> FieldState.String s |> Field.Leaf
            | None -> defaultNode

        {
            Validate = validate
            Schema = SchemaField.Leaf { Id = id; Label = None; Type = "string"; IsRequired = false }
            InitFrom = None
            Serialize = serialize
        }

    let private bindValidate
        (_type: string)
        (validate: 'a -> Result<'b, string -> (string list)>)
        (serialize: 'b -> 'a)
        (validator: Validator<'a, _, 'InitFrom>): Validator<'b, _, 'InitFrom> =
        let validate: Validate<'b, _> =
            fun (formFields: FormFields) env ->
                match validator.Validate formFields env with
                | Ok v ->
                    match validate v with
                    | Ok r -> Ok r
                    | Error e ->
                        let schemaId = Schema.getId env.Schema
                        let label =
                            Schema.getLabel env.Schema
                            |> Option.defaultValue schemaId
                        Error [ schemaId, label |> e ]
                | Error e -> Error e
        
        let schema = Schema.withType _type validator.Schema
                
        if Option.isSome validator.InitFrom then
            failwithf "Hydrate will the thrown away silently at %A" schema
                
        {
            Validate = validate
            Schema = schema
            InitFrom = None
            Serialize =
                fun env initSelector (value: 'b option) ->
                    let valueToSerialize =
                        match initSelector with
                        | Some initSelector ->
                            match initSelector env with
                            | Some value -> Some value
                            | None -> None
                        | None ->
                            match value with
                            | Some value -> Some value
                            | None -> None
                    
                    let serializedValue =  
                        match valueToSerialize with
                        | Some v -> Some (serialize v)
                        | None -> None
                        
                    validator.Serialize env None serializedValue
        }
        
    let private bindValidateO
        (_type: string)
        (validate: 'a -> Result<'b, string -> (string list)>)
        (serialize: 'b -> 'a)
        (validator: Validator<'a option, _, _>): Validator<'b option, _, _> =
        let validate (v: 'a option): Result<'b option, string -> string list> =
            match v with
            | Some v ->
                validate v
                |> Result.map Some
            | None -> Ok None
            
        let serialize (v: 'b option): 'a option =
            printfn "Serialize bind validate o"
            Option.map serialize v
            
        bindValidate
            _type
            validate
            serialize
            validator
        
    let rec asInt (validator: Validator<string option, _, _>): Validator<int option, _, _> =
        let validate (s: string) =
            match tryParseInt s with
            | Some i -> Ok i
            | None -> Error (fun label -> [ sprintf "%s should be a valid number" label ])
        
        let serialize (v: int) =
            string v
        
        bindValidateO
            "int"
            validate
            serialize
            validator
        
    and tryParseInt (s: string) =
        match System.Int32.TryParse(s) with
        | true, i -> Some i
        | _ -> None
    
    let withLabel (label: string) (validator: Validator<_, _, _>) =
        let schema = validator.Schema |> Schema.withLabel label
        { validator with Schema = schema }

    let isRequired (validator: Validator<'a option, _, _>): Validator<'a, _, _> =
        let schema = validator.Schema |> Schema.withIsRequired true
                    
        let validate: Validate<'a, _> =
            fun (formFields: FormFields) env ->
                match validator.Validate formFields env with
                | Ok (Some r) -> Ok r
                | Ok None ->
                    let schemaId = Schema.getId env.Schema
                    let label =
                        Schema.getLabel env.Schema
                        |> Option.defaultValue schemaId
                    Error [ schemaId, [ sprintf "%s is required" label ] ]
                | Error e -> Error e
        {
            Validate = validate
            Schema = schema
            InitFrom = None
            Serialize = fun env initSelector (value: 'a option) ->
                let valueToSerialize =
                    match initSelector with
                    | Some initSelector ->
                        match initSelector env with
                        | Some v -> Some v
                        | None -> None
                    | None ->
                        match value with
                        | Some v -> Some v
                        | None -> None

                let serializedValue =
                    match valueToSerialize with
                    | Some v -> Some (Some v)
                    | None -> Some None
                                
                validator.Serialize env None serializedValue
        }
                
    let initFrom (selector: 'Env -> 'a) (validator: Validator<'a, _, 'Env>) =
        {
            validator with InitFrom = Some (fun e -> selector e |> Some)
        }
            
    let mapInit (selector: 'EnvIn -> 'EnvOut) (validator: Validator<_, _,'EnvOut>): Validator<_, _, 'EnvIn> =
        let initFrom =
            match validator.InitFrom with
            | Some f ->
                Some (fun e -> f (selector e))
            | None -> None
        
        let serialize (env: 'EnvIn) (_: InitSelector<'EnvIn, _> option) _ =
            let mapped: 'EnvOut = selector env
            validator.Serialize mapped validator.InitFrom None
        {
            InitFrom = initFrom
            Schema = validator.Schema
            Validate = validator.Validate
            Serialize = serialize
        }

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
