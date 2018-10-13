module App

open Extensions
open Elmish
open Elmish.React
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.Core.JsInterop

type Field = string
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
    let validateFirstName (model: Model): ValidationResult<string> =
        let value = Form.getField firstNameId model
        if value.Length < 3 then 
            Error [ (firstNameId, "First name must be at least 3 characters") ]
        else 
            Ok value
    let validateLastName (model: Model): ValidationResult<string> =
        let value = Form.getField lastNameId model
        if value.Length < 1 || value.[0] <> 'a' then 
            Error [ (lastNameId, "Last name must begin with 'a'") ]
        else         
            Ok value
    let validateAge (model: Model): ValidationResult<int> =
        let value = Form.getField ageId model
        match tryParseInt value with 
        | Some age -> 
            if age <= 50 then
                Error [ (ageId, "Age must be above 50") ]
            else 
                Ok age            
        | None -> Error [ (ageId, "Age must be a number") ]
    let validateAddress1 (model: Model): ValidationResult<string> =
        let value = Form.getField address1Id model
        if value.Length > 0 then 
            Ok value
        else 
            Error [ (address1Id, "Address 1 is required")]        
    let validateAddress2 (model: Model): ValidationResult<string> =
        let value = Form.getField address2Id model
        if value.Length > 0 then 
            Ok value
        else 
            Error [ (address2Id, "Address 2 is required")]        

    let createAddress address1 address2 = 
        { Address1 = address1; Address2 = address2 }        

    let createPerson firstName lastName age address =
        { FirstName = firstName; LastName = lastName; Age = age; Address = address }    

    let validateAddress = Form.map2 createAddress

    let validatePerson = 
        Form.map4 createPerson validateFirstName validateLastName validateAge (validateAddress validateAddress1 validateAddress2)

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
        let showValidationMessageIfPresent = Form.isTouched fieldId model || IsSubmitted
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
