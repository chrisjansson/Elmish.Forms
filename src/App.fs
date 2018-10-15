module App

open Extensions
open Elmish
open Elmish.React
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.Core.JsInterop

type Field = string
type FieldName = string
type FieldId = string

type ValidationError = string
type KeyedValidationError = FieldId * ValidationError
type ValidationResult<'a> = Result<'a, KeyedValidationError list>

type Person = {
    FirstName: string
    LastName: string
    Age: int
    Address: Address
}
and Address = {
    Address1: string
    Address2: string
}

type Model =
    {
        Fields: Map<FieldId, Field>
        ValidationErrors: Map<FieldId, ValidationError list>
        Touched: Set<FieldId>
        Result: Person option
        IsSubmitted: bool
    }

type Validator<'a> = (Model -> ValidationResult<'a>)

type FieldDefinition<'a> = 
    { 
        Id: FieldId
        Name: string
        Validate: Validator<'a>
    }

type Msg =
    | InputChanged of FieldId * string
    | Touch of FieldId
    | Submit

let init() : Model = {
    Fields = Map.empty
    ValidationErrors = Map.empty
    Result = None    
    Touched = Set.empty
    IsSubmitted = false
}

module FieldId =
    let create (id: string): FieldId = id

module Field =
    let defaultValue: Field = ""




module Form =


    let getField (id: FieldId) (model: Model): Field = 
        Map.tryFind id model.Fields 
        |> Option.defaultValue Field.defaultValue

    let getValidationErrors (id: FieldId) (model: Model): ValidationError list =
        Map.tryFind id model.ValidationErrors
        |> Option.defaultValue []

    let hasValidationError (id: FieldId) (model: Model): bool =
        getValidationErrors id model |> List.isEmpty |> not

    let isTouched (id: FieldId) (model: Model): bool =
        Set.contains id model.Touched

    let map2 f a b (model: Model) =
        let fa = Result.lift2 f
        let a' = a model
        let b' = b model
        fa a' b'

    let map3 f a b c (model: Model) =
        let fa = Result.lift3 f
        let a' = a model
        let b' = b model
        let c' = c model
        fa a' b' c'

    let map4 f a b c d (model: Model) =
        let fa = Result.lift4 f
        let a' = a model
        let b' = b model
        let c' = c model
        let d' = d model
        fa a' b' c' d'

module FieldDefinition =
    let private createDefaultOptionalFieldValidator (id: FieldId) =
        fun model -> 
            let field = Form.getField id model
            if field.Length = 0 then
                None |> Ok
            else    
                Some field |> Ok

    let define (id: string): FieldDefinition<string option> =
        let fieldId: FieldId = FieldId.create id
        {
            Id = fieldId
            Name = id
            Validate = createDefaultOptionalFieldValidator fieldId
        }

    let withName (name: FieldName) (field: FieldDefinition<_>)  =
        { field with Name = name }

    let isRequired (field: FieldDefinition<_>) =
        let requiredValidation model =
            match field.Validate model with
            | Ok (Some v) -> Ok v
            | Ok None -> 
                let requiredErrorMessage = sprintf "%s is required" field.Name
                Error [ (field.Id, requiredErrorMessage) ]
            | Error e -> Error e
        { 
            Id = field.Id
            Name = field.Name
            Validate = requiredValidation 
        }        

    let minimumLength (length: int) (field: FieldDefinition<string option>) =
        let lengthValidation model =
            match field.Validate model with
            | Ok (Some v) -> 
                if v.Length >= length then
                    v |> Some |> Ok
                else                 
                    let errorMessage = sprintf "%s must be at least %i characters" field.Name length
                    Error [ (field.Id, errorMessage) ]
            | Ok None -> Ok None                
            | Error e -> Error e            
        {
            Id = field.Id
            Name = field.Name
            Validate = lengthValidation
        }        

    let int (field: FieldDefinition<string option>) =
        let intValidation model =
            match field.Validate model with
            | Ok (Some v) -> 
                match System.Int32.TryParse(v) with
                | (true, i) -> i |> Some |> Ok
                | _ -> 
                    let errorMessage = sprintf "%s must be an integer" field.Name
                    Error [ (field.Id, errorMessage) ]
            | Ok None -> Ok None                
            | Error e -> Error e            
        {
            Id = field.Id
            Name = field.Name
            Validate = intValidation
        }        

    let greaterThan (limit: int) (field: FieldDefinition<int option>) =
        let limitValidation model =
            match field.Validate model with
            | Ok (Some v) -> 
                if v >= limit then
                    v |> Some |> Ok
                else                 
                    let errorMessage = sprintf "%s must be at greater than %i" field.Name limit
                    Error [ (field.Id, errorMessage) ]
            | Ok None -> Ok None                
            | Error e -> Error e            
        {
            Id = field.Id
            Name = field.Name
            Validate = limitValidation
        }        

    let predicate (pred: 'a -> bool) (buildMessage: string -> string) (field: FieldDefinition<'a option>) =
        let validate model =
            match field.Validate model with
            | Ok (Some v) ->
                if pred v then
                    v |> Some |> Ok
                else
                    let errorMessage = buildMessage field.Name
                    Error [ (field.Id, errorMessage) ]                
            | Ok None -> Ok None                
            | Error e -> Error e                
        {
            Id = field.Id
            Name = field.Name
            Validate = validate
        }        



let firstNameId = FieldId.create "firstName"
let lastNameId = FieldId.create "lastName"
let ageId = FieldId.create "age"
let address1Id = FieldId.create "address1"
let address2Id = FieldId.create "address2"

let tryParseInt (s: string) =
    match System.Int32.TryParse(s) with
    | (true, i) -> Some i
    | _ -> None

let validate (model: Model): ValidationResult<Person> = 
    let validateFirstName =
        FieldDefinition.define firstNameId
        |> FieldDefinition.withName "First name"
        |> FieldDefinition.minimumLength 3
        |> FieldDefinition.isRequired

    let validateLastName (model: Model): ValidationResult<string> =
        let value = Form.getField lastNameId model
        if value.Length < 1 || value.[0] <> 'a' then 
            Error [ (lastNameId, "Last name must begin with 'a'") ]
        else         
            Ok value

    let validateAge =
        FieldDefinition.define ageId        
        |> FieldDefinition.withName "Age"
        |> FieldDefinition.int
        |> FieldDefinition.greaterThan 50
        |> FieldDefinition.isRequired

    let validateAddress1 =
        FieldDefinition.define address1Id
        |> FieldDefinition.withName "Address 1"
        |> FieldDefinition.isRequired

    let validateAddress2 =
        FieldDefinition.define address2Id
        |> FieldDefinition.withName "Address 2"
        |> FieldDefinition.isRequired

    let createAddress address1 address2 = 
        { Address1 = address1; Address2 = address2 }        

    let createPerson firstName lastName age address =
        { FirstName = firstName; LastName = lastName; Age = age; Address = address }    

    let validateAddress = Form.map2 createAddress

    let validatePerson = 
        Form.map4 
            createPerson 
            validateFirstName.Validate 
            validateLastName 
            validateAge.Validate 
            (validateAddress validateAddress1.Validate validateAddress2.Validate)

    validatePerson model

let update (msg:Msg) (model:Model) =
    let applyValidation (model: Model) (validationResult: ValidationResult<Person>) =    
        let appendError (map: Map<FieldId, ValidationError list>) (error: KeyedValidationError): Map<FieldId, ValidationError list> =
            let key = fst error
            let validationError = snd error
            if Map.containsKey key map then
                let lst = Map.find key map
                Map.add key (validationError :: lst) map
            else
                Map.add key [ validationError ] map
        let validationErrorMap = 
            match validationResult with
            | Ok _ -> Map.empty
            | Error validationErrors -> validationErrors |> List.fold appendError Map.empty 
        { model with ValidationErrors = validationErrorMap }

    let validateModel (validate: Validator<Person>) (model: Model) = validate model |> applyValidation model

    match msg with
    | InputChanged (id, value) -> 
        let model = { model with Fields = Map.add id value model.Fields }
        validateModel validate model
    | Touch id -> 
        let touched = Set.add id model.Touched
        let model = { model with Touched = touched }
        validateModel validate model
    | Submit -> 
        let model = { model with IsSubmitted = true }
        let validationResult = validate model
        let model = 
            match validationResult with
            | Ok r -> { model with Result = Some r }
            | Error _ -> { model with Result = None }
        applyValidation model validationResult        

let view (model:Model) dispatch =
    let onChange field (event: Fable.Import.React.FormEvent) =
        let value = string event.currentTarget?value
        InputChanged (field, value) |> dispatch

    let onSubmit (event: Fable.Import.React.FormEvent) = 
        event.preventDefault()
        dispatch Submit    

    let onBlur fieldId _ =
        Touch fieldId |> dispatch 

    let validationLabelFor (fieldId: FieldId) (model: Model) =    
        let showValidationMessageIfPresent = Form.isTouched fieldId model || model.IsSubmitted
        if Form.hasValidationError fieldId model && showValidationMessageIfPresent then
            let message = 
                Form.getValidationErrors fieldId model
                |> List.reduce (fun acc v -> acc + " " + v)
            label [] [ unbox message ]
        else 
            fragment [] []

    let formInput (labelText: string) fieldId =     
        div [] [
            label [] [ unbox labelText ]
            input [ Form.getField fieldId model |> Value; onChange fieldId |> OnChange; onBlur fieldId |> OnBlur ]
            validationLabelFor fieldId model
        ]
    
    div []
        [
            form [ OnSubmit onSubmit ] [
                formInput "First name" firstNameId
                formInput "Last name" lastNameId
                formInput "Age" ageId
                formInput "Address 1" address1Id
                formInput "Address 2" address2Id
                button [ Type "submit" ] [ unbox "Submit" ]
            ]

            pre [] [
                (match model.Result with
                | Some r -> unbox (sprintf "%A" r)
                | None -> unbox "")
            ]
        ]

Program.mkSimple init update view
    |> Program.withReact "elmish-app"
    |> Program.withConsoleTrace
    |> Program.run
