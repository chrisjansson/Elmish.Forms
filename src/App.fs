module App

open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.Core.JsInterop
open Forms
open Forms.Model

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

type Model = Forms.Model.Model<Person>

let firstNameId = FieldId.create "firstName"
let lastNameId = FieldId.create "lastName"
let ageId = FieldId.create "age"
let address1Id = FieldId.create "address1"
let address2Id = FieldId.create "address2"

let tryParseInt (s: string) =
    match System.Int32.TryParse(s) with
    | (true, i) -> Some i
    | _ -> None
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
//    let createAddress address1 address2 = 
//        { Address1 = address1; Address2 = address2 }        
//
//    let createPerson firstName lastName age address =
//        { FirstName = firstName; LastName = lastName; Age = age; Address = address }    
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
//
//let update (msg:Msg) (model:Model) =
//    let applyValidation (model: Model) (validationResult: ValidationResult<Person>) =    
//        let appendError (map: Map<FieldId, ValidationError list>) (error: KeyedValidationError): Map<FieldId, ValidationError list> =
//            let key = fst error
//            let validationError = snd error
//            if Map.containsKey key map then
//                let lst = Map.find key map
//                Map.add key (validationError :: lst) map
//            else
//                Map.add key [ validationError ] map
//        let validationErrorMap = 
//            match validationResult with
//            | Ok _ -> Map.empty
//            | Error validationErrors -> validationErrors |> List.fold appendError Map.empty 
//        { model with ValidationErrors = validationErrorMap }
//
//    let validateModel (validate: Validator<Person, Person>) (model: Model) = validate model |> applyValidation model
//
//    match msg with
//    | InputChanged (id, value) -> 
//        // let model = { model with Fields = Map.add id value model.Fields }
//        // validateModel validate model
//        failwith "todo"
//    | Touch id -> 
//        let touched = Set.add id model.Touched
//        let model = { model with Touched = touched }
//        validateModel validate model
//    | Submit -> 
//        let model = { model with IsSubmitted = true }
//        let validationResult = validate model
//        let model = 
//            match validationResult with
//            | Ok r -> { model with Result = Some r }
//            | Error _ -> { model with Result = None }
//        applyValidation model validationResult        
//
//let view (model:Model) dispatch =
//    let onChange field (event: Fable.Import.React.FormEvent) =
//        let value = string event.currentTarget?value
//        InputChanged (field, value) |> dispatch
//
//    let onSubmit (event: Fable.Import.React.FormEvent) = 
//        event.preventDefault()
//        dispatch Submit    
//
//    let onBlur fieldId _ =
//        Touch fieldId |> dispatch 
//
//    let validationLabelFor (fieldId: FieldId) (model: Model) =    
//        let showValidationMessageIfPresent = Form.isTouched fieldId model || model.IsSubmitted
//        if Form.hasValidationError fieldId model && showValidationMessageIfPresent then
//            let message = 
//                Form.getValidationErrors fieldId model
//                |> List.reduce (fun acc v -> acc + " " + v)
//            label [] [ unbox message ]
//        else 
//            fragment [] []
//
//    let formInput (labelText: string) fieldId =     
//        div [] [
//            label [] [ unbox labelText ]
//            input [ Form.getField fieldId model |> Value; onChange fieldId |> OnChange; onBlur fieldId |> OnBlur ]
//            validationLabelFor fieldId model
//        ]
//    
//    div []
//        [
//            form [ OnSubmit onSubmit ] [
//                formInput "First name" firstNameId
//                formInput "Last name" lastNameId
//                formInput "Age" ageId
//                formInput "Address 1" address1Id
//                formInput "Address 2" address2Id
//                button [ Type "submit" ] [ unbox "Submit" ]
//            ]
//
//            pre [] [
//                (match model.Result with
//                | Some r -> unbox (sprintf "%A" r)
//                | None -> unbox "")
//            ]
//        ]

// Program.mkSimple init update view
//     |> Program.withReact "elmish-app"
//     |> Program.withConsoleTrace
//     |> Program.run

Path.Tests.runTests ()
FormTests.run ()
