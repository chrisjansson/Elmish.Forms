[<AutoOpen>]
module Elmish.Forms.Types

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

//Model types
[<RequireQualifiedAccess>]
type Field =
    | Group of FieldGroup
    | List of FieldList
    | Leaf of FieldState
    
and FieldGroup = Map<FieldId, Field>
and FieldList = FieldGroup list //TODO: change to Field list?
and FieldId = string
and [<RequireQualifiedAccess>] FieldState =
    | String of string

[<RequireQualifiedAccess>]
type SchemaField =
    | Leaf of LeafMetaData
    | Group of GroupMetaData
    | Type of TypeMetaData 
    | Sub of SubMetaData
    | List of ListMetaData 
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
        Fields: Map<FieldId, SchemaField>
    }
and TypeMetaData =
    {
        Type: string
        Label: string option
        Fields: Map<FieldId, SchemaField>
    }
and SubMetaData =
    {
        Id: FieldId
        SubSchema: SchemaField
    }
and ListMetaData =
    {
        Id: FieldId
        SubSchema: SchemaField
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
        Schema: SchemaField
    }

[<RequireQualifiedAccess>]
type Path =
    | List of string * int
    | Node of string