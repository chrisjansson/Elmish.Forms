module App

open Elmish
open Elmish.React
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.Core.JsInterop

type ResultBuilder() =
    member this.Bind(x, f) =
        match x with
        | Ok x -> f(x) 
        | Error e -> Error e
    member this.Delay(f) = f()
    member this.Return(x) = Ok x

let result = new ResultBuilder()

module Result =
    // M(a -> b) -> M a -> M b
    let apply fr ar =
        match (fr, ar) with
        | (Ok f, Ok a) -> f a |> Ok
        | (Error e, Ok _) -> Error e
        | (Ok _, Error e) -> Error e
        | (Error e1, Error e2) -> List.concat [ e1; e2 ] |> Error

    let (<*>) f a = apply f a

    let ret a = Ok a    

    let lift2 f a b = ret f <*> a <*> b
    let lift3 f a b c = ret f <*> a <*> b <*> c
    let lift4 f a b c d = ret f <*> a <*> b <*> c <*> d

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
        Result: Person option
    }

type Validator<'a> = (Model -> ValidationResult<'a>)

type Msg =
    | InputChanged of FieldId * string
    | Submit


let init() : Model = {
    Fields = Map.empty
    ValidationErrors = Map.empty
    Result = None    
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
    let validateModel (validate: Validator<Person>) (model: Model) = 
        let validationResult = validate model 
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

    match msg with
    | InputChanged (id, value) -> 
        let model = { model with Fields = Map.add id value model.Fields }
        validateModel validate model
    | Submit -> 
        match validate model with
        | Ok r -> { model with Result = Some r }
        | Error _ -> { model with Result = None }

let view (model:Model) dispatch =
    let onChange field (event: Fable.Import.React.FormEvent) =
        let value = string event.currentTarget?value
        InputChanged (field, value) |> dispatch

    let onSubmit (event: Fable.Import.React.FormEvent) = 
        event.preventDefault()
        dispatch Submit    

    let validationLabelFor (fieldId: FieldId) (model: Model) =    
        if Form.hasValidationError fieldId model then
            let message = 
                Form.getValidationErrors fieldId model
                |> List.reduce (fun acc v -> acc + " " + v)
            label [] [ unbox message ]
        else 
            fragment [] []
    
    div []
        [
            form [ OnSubmit onSubmit ] [
                div [] [
                    label [] [ unbox "First name" ]
                    input [ Form.getField firstNameId model |> Value; onChange firstNameId |> OnChange ]
                    validationLabelFor firstNameId model
                ]
                div [] [
                    label [] [ unbox "Last name" ]
                    input [ Form.getField lastNameId model |> Value; onChange lastNameId |> OnChange ]
                    validationLabelFor lastNameId model 
                ]
                div [] [
                    label [] [ unbox "Age" ]
                    input [ Form.getField ageId model |> Value; onChange ageId |> OnChange ]
                    validationLabelFor ageId model 
                ]
                div [] [
                    label [] [ unbox "Address 1" ]
                    input [ Form.getField address1Id model |> Value; onChange address1Id |> OnChange ]
                    validationLabelFor address1Id model 
                ]
                div [] [
                    label [] [ unbox "Address 2" ]
                    input [ Form.getField address2Id model |> Value; onChange address2Id |> OnChange ]
                    validationLabelFor address2Id model 
                ]
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
