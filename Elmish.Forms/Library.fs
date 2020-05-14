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
    and LeafMetaData =
        {
            Id: FieldId
            Label: string option
            Type: string
            IsRequired: bool
        }
    
    //Validator structure
    and ValidationResult<'Result> = Result<'Result, ValidationErrors>
    and KeyedValidationError = FieldId * string list
    and ValidationErrors = KeyedValidationError list
    and ValidationErrorContext = unit
    and Validate<'Result, 'Env> = FormFields -> Context<'Env> -> ValidationResult<'Result>
    and Context<'Env> = { Env: 'Env; Schema: SchemaField }
    and InitSelector<'Env, 'Result> = ('Env -> 'Result)
    and Validator<'Result, 'Env, 'InitializeFrom> =
        {
            Validate: Validate<'Result, 'Env>
            Schema: SchemaField
            InitFrom: InitSelector<'Env, 'Result> option
            Serialize: 'Result -> FieldState
        }

    and FormFields = FieldGroup
    
    type Model =
        {
            FormFields: FieldGroup
        }

module Validators =
    open Core

    module Schema =
        
        
        let getId (schema: SchemaField) =
            match schema with
            | SchemaField.Leaf ld -> ld.Id
            
        let getLabel (schema: SchemaField) =
            match schema with
            | SchemaField.Leaf ld -> ld.Label
            
        let withType (t: string) (schema: SchemaField) =
            match schema with
            | SchemaField.Leaf ld -> SchemaField.Leaf { ld with Type = t }

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
                     
        let serialize (value: string option) =
            match value with
            | Some s -> FieldState.String s
            | None -> FieldState.String ""
                     
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
        (validator: Validator<'a, _, _>): Validator<'b, _, _> =
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
            Serialize = fun v -> serialize v |> validator.Serialize 
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
        let schema =
            match validator.Schema with
            | SchemaField.Leaf leafData ->
                SchemaField.Leaf
                    {
                        leafData with Label = Some label
                    }
        { validator with Schema = schema }

    let isRequired (validator: Validator<'a option, _, _>): Validator<'a, _, _> =
        let schema =
            match validator.Schema with
            | SchemaField.Leaf leafData ->
                SchemaField.Leaf
                    {
                        leafData with IsRequired = true
                    }
                    
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
            Serialize = fun s -> validator.Serialize (Some s)
        }
        
    let initFrom (selector: 'Env -> 'a) (validator: Validator<'a, _, 'Env>) =
        {
            validator with InitFrom = Some selector
        }

module Form =
    open Core
    
    let init (validator: Validator<_, _, _>): Model =
        let data = 
            match validator.Schema with
            | SchemaField.Leaf ld ->
                Map.ofSeq [ ld.Id, Field.Leaf (FieldState.String "") ]
        
        {
            FormFields = data
        }
        
    let initWithDefault (validator: Validator<_, _, 'Env>) (env: 'Env): Model =
        let data = 
            match validator.Schema with
            | SchemaField.Leaf ld ->
                let fieldValue =
                    match validator.InitFrom with
                    | Some init -> init env |> validator.Serialize
                    | None -> FieldState.String ""
                
                Map.ofSeq [ ld.Id, Field.Leaf fieldValue ]
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
