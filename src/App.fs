module App

open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.Core.JsInterop
open ElmishForm
open Elmish
open Elmish.React

type Person = {
    FirstName: string
    LastName: string
    Age: int
    Address: Address
    Pets: Pets
}
and Address = {
    Address1: string
    Address2: string
}

and Pets = string list

type Model = Form.Model<Person>

let firstNameId = "firstName"
let lastNameId = "lastName"
let ageId = "age"
let address1Id = "address.address1"
let address2Id = "address.address2"

let tryParseInt (s: string) =
    match System.Int32.TryParse(s) with
    | (true, i) -> Some i
    | _ -> None
    
let createPerson firstName lastName age address pets =
    { FirstName = firstName; LastName = lastName; Age = age; Address = address; Pets = pets }    

open ElmishForm.Form.Validator
open ElmishForm.Form

let createAddress address1 address2 =
    printfn "Create address!!"
    { Address1 = address1; Address2 = address2 }    

let pet = Validator.text "name" |> Validator.required

let address =
    Validator.from createAddress
    <*> (Validator.text "address1" |> Validator.required |> withLabel "Address 1")
    <*> (Validator.text "address2" |> Validator.required |> withLabel "Address 2")

let person =
    Validator.from createPerson
    <*> (Validator.text "firstName" |> Validator.required |> withLabel "First name")
    <*> (Validator.text "lastName" |> Validator.required |> withLabel "Last name")
    <*> (Validator.text "age" |> Validator.asInt |> Validator.required |> withLabel "Age")
    <*> (Validator.withSub "address" address)
    <*> (Validator.withList "pets" pet)

    
//
//let validate (model: Model): ValidationResult<Person> = 
//    let validateFirstName =
//        FieldDefinition.define firstNameId
//        |> FieldDefinition.withName "First name"
//        |> FieldDefinition.minimumLength 3
//        |> FieldDefinition.isRequired
//
//    let validateLastName =
//        let startsWithA (s: string) =  s.Length < 1 || s.[0] <> 'a'
//
//        FieldDefinition.define lastNameId        
//        |> FieldDefinition.withName "Last name"
//        |> FieldDefinition.predicate startsWithA (fun fieldName -> sprintf "%s must begin with 'a'" fieldName) 
//        |> FieldDefinition.isRequired
//
//    let validateAge =
//        FieldDefinition.define ageId        
//        |> FieldDefinition.withName "Age"
//        |> FieldDefinition.int
//        |> FieldDefinition.greaterThan 50
//        |> FieldDefinition.isRequired
//
//    let validateAddress1 =
//        FieldDefinition.define address1Id
//        |> FieldDefinition.withName "Address 1"
//        |> FieldDefinition.isRequired
//
//    let validateAddress2 =
//        FieldDefinition.define address2Id
//        |> FieldDefinition.withName "Address 2"
//        |> FieldDefinition.isRequired    
//
//
//    let validateAddress = Form.map2 createAddress
//
//    let validatePerson = 
//        Form.map4 
//            createPerson 
//            validateFirstName.Validate 
//            validateLastName.Validate 
//            validateAge.Validate 
//            (validateAddress validateAddress1.Validate validateAddress2.Validate)
//
//    validatePerson model
open Form

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

    let validateModel validate (model: Model) =
        Validator.run validate model |> applyValidation model

    match msg with
    | InputChanged (id, value) -> 
        Form.setFieldValue id model value
        |> validateModel person
    | Touch id -> 
        let touched = Set.add id model.Touched
        let model = { model with Touched = touched }
        validateModel person model
    | Submit -> 
        let model = { model with IsSubmitted = true }
        let validationResult = Validator.run person model
        let model = 
            match validationResult with
            | Ok r -> { model with Result = Some r }
            | Error _ -> { model with Result = None }
        applyValidation model validationResult
    | AppendList id ->
        Form.appendListItem id model
        |> validateModel person
    | RemoveListItem (id, index) ->
        Form.removeListItem id index model
        |> validateModel person

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
                |> List.fold (fun acc v -> acc + " " + v) " "
            label [] [ unbox message ]
        else 
            fragment [] []

    let formInput (labelText: string) fieldId =     
        div [] [
            label [] [ unbox labelText ]
            input [ Form.getFieldValue fieldId model |> Value; onChange fieldId |> OnChange; onBlur fieldId |> OnBlur ]
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
                button [ Type "button"; OnClick (fun _ -> dispatch (AppendList "pets")) ] [ str "Add pet" ]
                div [] [
                    yield h1 [] [str "Pets"]
                    for i = 0 to ((Form.getListLength "pets" model) - 1) do
                        yield formInput "Name" ("pets.[" + string i + "].name")
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
    |> Program.runWith person
//
Path.Tests.runTests ()
FormTests.run ()
