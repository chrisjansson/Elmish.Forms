namespace Elmish.Forms

//Validation error formatter
// 'ValidationErrorContext -> string list

//A validation function takes a validation environment, the value and returns a new value
// 'value_in -> 'env -> Result<'value_out, ValidationErrorFormatter>

//A validator
//Validator<'Result, 'Env, 'InitializeFrom>
// {
//    Validate: FormData -> 'Env -> Result
//    Schema: A description of the forms structure, includes metadata such as labels, type, requiredness, static default values
//
//
//
//
// }

//InitFrom from note, when combining a validator like
// text "abc" |> initFrom (fun x -> x.A) |> asInt
// In this case the 'InitFrom expected by asInt would type missmatches
// The current solution is moving text "abc" |> asInt |> initFrom (fun x -> x.A)
// In the case of "abc" |> initFrom (fun x -> x) |> asInt |> initFrom (fun x -> x.A) throw an exception from the asInt combinator since the first initFrom will be thrown away silently

module Core =
    
    
    
    //Model types
    [<RequireQualifiedAccess>]
    type Field =
        | Group of FieldGroup
        | List of FieldList
        | Leaf of FieldState
        
    and FieldGroup = Map<FieldId, Field>
    and FieldList = FieldGroup list * FieldGroup
    and FieldId = string
    and [<RequireQualifiedAccess>] FieldState =
        | String of string
    
    [<RequireQualifiedAccess>]
    type SchemaField =
        | Leaf of LeafMetaData
        | Group of GroupMetaData
    and LeafMetaData =
        {
            Id: FieldId
            Label: string option
            Type: string
            IsRequired: bool
        }
    and GroupMetaData =
        {
            Type: string
            Label: string option
        }
    
    //Validator structure
    and ValidationResult<'Result> = Result<'Result, ValidationErrors>
    and KeyedValidationError = FieldId * string list
    and ValidationErrors = KeyedValidationError list
    and ValidationErrorContext = unit
    and Validate<'Result, 'Env> = FormFields -> Context<'Env> -> ValidationResult<'Result>
    and Context<'Env> = { Env: 'Env; Schema: SchemaField }
    and InitSelector<'Env, 'Result> = ('Env -> 'Result option)
    and Validator<'Result, 'Env, 'InitializeFrom> =
        {
            Validate: Validate<'Result, 'Env>
            Schema: SchemaField
            InitFrom: InitSelector<'InitializeFrom, 'Result> option
            Serialize: 'InitializeFrom -> InitSelector<'InitializeFrom, 'Result> option -> 'Result option -> Field
        }

    and FormFields = FieldGroup
    
    type Model =
        {
            FormFields: FieldGroup
        }

module Validator =
    open Core

    let from (f: 'T): Validator<'T, 'Env, 'InitFrom> =
        let validate _ (_: Context<'Env>) =
            Ok f
        {
            Validate = validate
            Schema = SchemaField.Leaf { Id = ""; Type = sprintf "Custom validator form %s" (typeof<'T>.Name); IsRequired = false; Label = None } //TODO: Is leaf really correct here?
            InitFrom = None
            Serialize = fun _ _ _ -> Field.Group (Map.empty)
        }

    
    module Schema =
        let getId (schema: SchemaField) =
            match schema with
            | SchemaField.Leaf ld -> ld.Id
            | SchemaField.Group _ -> failwith "Group has no id"
            
        let getLabel (schema: SchemaField) =
            match schema with
            | SchemaField.Leaf ld -> ld.Label
            | SchemaField.Group gd -> gd.Label
            
        let withLabel (label: string) (schema: SchemaField) =
            match schema with
            | SchemaField.Leaf ld ->
                SchemaField.Leaf
                    {
                        ld with Label = Some label
                    }
            | SchemaField.Group gd ->
                SchemaField.Group
                    {
                        gd with Label = Some label
                    }
                    
        let withIsRequired (isRequired: bool) (schema: SchemaField) =
            match schema with
            | SchemaField.Leaf ld ->
                SchemaField.Leaf
                    {
                        ld with IsRequired = isRequired
                    }
            | SchemaField.Group _ -> schema
            
        let getType (schema: SchemaField) =
            match schema with
            | SchemaField.Leaf ld -> ld.Type
            | SchemaField.Group gd -> gd.Type
            
        let withType (t: string) (schema: SchemaField) =
            match schema with
            | SchemaField.Leaf ld -> SchemaField.Leaf { ld with Type = t }
            | SchemaField.Group gd -> SchemaField.Group { gd with Type = t }
    
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
            | _ -> failwith "Unknown apply combination"
            
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
            
            
//            match initSelector, env with
//            | Some initSelector, Some env ->
//                let env = initSelector env
//                match env with
//                | Some env ->
//                    let hv1 = vf.Serialize vf.InitFrom (Some env)
//                    let hv2 = va.Serialize va.InitFrom (Some env)
//                    joinGroups hv1 hv2
//                | None ->
//                    let hv1 = vf.Serialize vf.InitFrom None
//                    let hv2 = va.Serialize va.InitFrom None
//                    joinGroups hv1 hv2
//            | _ -> 
//                    let hv1 = vf.Serialize vf.InitFrom None
//                    let hv2 = va.Serialize va.InitFrom None
//                    joinGroups hv1 hv2
        {
            Validate = validate
            InitFrom = None
            Serialize = serialize
            Schema = SchemaField.Group { Label = None; Type = sprintf "Applicative of %s and %s" (Schema.getType va.Schema) (Schema.getType vf.Schema) }
        }

module Validators =
    open Core
    open Validator

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
                            
//                    match initSelector, env with
//                    | Some initSelector, Some env -> 
//                        let mappedValue = initSelector env
//                        match mappedValue with
//                        | Some value ->
//                            serialize value
//                            |> Some
//                            |> validator.Serialize validator.InitFrom
//                        | None -> validator.Serialize validator.InitFrom None
//                    | None, Some env ->
//                        serialize env
//                        |> Some
//                        |> validator.Serialize validator.InitFrom
//                    | _ -> validator.Serialize validator.InitFrom None
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

module Form =
    open Core
    
    let init (validator: Validator<_, _, _>): Model =
        let data = 
            match validator.Schema with
            | SchemaField.Leaf ld ->
                let value = validator.Serialize () validator.InitFrom None
                Map.ofSeq [ ld.Id, value ]
            | SchemaField.Group _ ->
                let value = validator.Serialize () validator.InitFrom None
                match value with
                | Field.Group gd -> gd
                | _ -> failwith "should serialize to gd"
        {
            FormFields = data
        }
        
    let initWithDefault (validator: Validator<_, _, 'Env>) (env: 'Env): Model =
        let data = 
            match validator.Schema with
            | SchemaField.Leaf ld ->
                let value = validator.Serialize env validator.InitFrom None
                Map.ofSeq [ ld.Id, value ]
            | SchemaField.Group _ ->
                let value = validator.Serialize env validator.InitFrom None
                match value with
                | Field.Group gd -> gd
                | _ -> failwith "should serialize to gd"
        {
            FormFields = data
        }
        
    let setField (id: FieldId) (value: FieldState) (model: Model) =
        let newFormFields = 
            model.FormFields
            |> Map.find id
            |> (fun _ -> Map.add id (Field.Leaf value) model.FormFields)        
        { model with FormFields = newFormFields }
        
    let validate (validator: Validator<'r, 'env, _>) (env: 'env) (formFields: FormFields): ValidationResult<'r> =
        validator.Validate formFields { Env = env; Schema = validator.Schema } 
