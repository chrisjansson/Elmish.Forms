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
    and Validate<'Result, 'Env> = FormFields -> ValidationContext<'Env> -> ValidationResult<'Result>
    and ValidationContext<'Env> = { Env: 'Env; Schema: SchemaField } 
    and Validator<'Result, 'Env, 'InitializeFrom> =
        {
            Validate: Validate<'Result, 'Env>
            Schema: SchemaField
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
                     
        {
            Validate = validate
            Schema = SchemaField.Leaf { Id = id; Label = None; Type = "string"; IsRequired = false }
        }

    let private bindValidate (_type: string) (validate: 'a -> Result<'b, string -> (string list)>) (validator: Validator<'a, _, _>): Validator<'b, _, _> =
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
        {
            Validate = validate
            Schema = Schema.withType _type validator.Schema
        }
        
    let private bindValidateO (_type: string) (validate: 'a -> Result<'b, string -> (string list)>) (validator: Validator<'a option, _, _>): Validator<'b option, _, _> =
        let validate (v: 'a option): Result<'b option, string -> string list> =
            match v with
            | Some v ->
                validate v
                |> Result.map Some
            | None -> Ok None
            
        bindValidate _type validate validator
        
    let rec asInt (validator: Validator<string option, _, _>): Validator<int option, _, _> =
        let validate (s: string) =
            match tryParseInt s with
            | Some i -> Ok i
            | None -> Error (fun label -> [ sprintf "%s should be a valid number" label ])
        bindValidateO "int" validate validator
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
        
    let setField (id: FieldId) (value: FieldState) (model: Model) =
        let newFormFields = 
            model.FormFields
            |> Map.find id
            |> (fun _ -> Map.add id (Field.Leaf value) model.FormFields)        
        { model with FormFields = newFormFields }
        
    let validate (validator: Validator<'r, 'env, _>) (env: 'env) (formFields: FormFields): ValidationResult<'r> =
        validator.Validate formFields { Env = env; Schema = validator.Schema } 
