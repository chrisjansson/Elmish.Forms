module App.Samples.ApplicativeSample


open Elmish.Forms
open Elmish.Forms.Validator.Operators
open Feliz

let source: string = Fable.Core.JsInterop.importDefault (Utils.importLiteral + __SOURCE_FILE__)

type Person =
    {
        FirstName: string
        MiddleName: string option
        LastName: string
    }

let validator (): Validator<Person, unit, _> =
    Validator.from (fun firstName middleName lastName ->
            {
                FirstName = firstName
                LastName = lastName
                MiddleName = middleName
            }
        )
    <*> (Validator.Standard.text "firstName" |> Validator.withLabel "First name" |> Validator.isRequired)
    <*> (Validator.Standard.text "middleName" |> Validator.withLabel "Middle name")
    <*> (Validator.Standard.text "lastName" |> Validator.withLabel "Last name" |> Validator.isRequired)
    
let render () =
    React.fragment [
        Gui.input "firstName"
        Gui.input "middleName"
        Gui.input "lastName"
    ]