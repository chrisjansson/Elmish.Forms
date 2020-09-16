module Elmish.Forms.Validator

let from (f: 'T): Validator<'T, 'Env, 'InitFrom> =
    let validate _ (_: Context<'Env>) =
        Ok f
    {
        Validate = validate
        Schema = SchemaField.Type { Type = sprintf "Custom validator form %s" (typeof<'T>.Name); Label = None; Fields = Map.empty }
        InitFrom = None
        Serialize = fun _ _ _ -> Field.Group (Map.empty)
    }
    
let bind
    (validate: 'In -> Context<'Env> -> ValidationResult<'Result>)
    (serialize: 'Result -> 'In)
    (validator: Validator<'In, 'Env, 'InitFrom>): Validator<'Result, 'Env, 'InitFrom> =
    let validate formFields (context: Context<'Env>) =
        match validator.Validate formFields context with
        | Ok r -> validate r context
        | Error e -> Error e
    { 
        Validate = validate
        Schema = validator.Schema
        InitFrom = None
        Serialize =
            fun env initSelector (value: _ option) ->
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
        
    let serialize env (_: InitSelector<_, _> option) _ =
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

            let schema =
                match context.Schema with
                | SchemaField.List l -> l.SubSchema
                | _ -> failwith "expected list schema"
            
            let context =
                { context with Schema = schema }
            
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
    

module Standard = 
        
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
                    | _ -> failwith "Not implemented"
                | None -> failwith "Not implemented"
                     
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

    let bindValidate
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
        
    let bindValidateO
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