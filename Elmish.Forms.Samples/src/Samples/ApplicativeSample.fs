module App.Samples.ApplicativeSample


open Elmish.Forms
open Elmish.Forms.Validator.Operators
open Elmish.Forms.React
open Feliz

let source: string = Fable.Core.JsInterop.importDefault (Utils.importLiteral + __SOURCE_FILE__)

type Person =
    {
        FirstName: string
        MiddleName: string option
        LastName: string
    }


let validator: Validator<Person, unit, unit> =
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
    let input (id: FieldId) =
        let field = useField id

        Html.div [
            let label = field.Label

            Html.div [
                Html.label [
                    prop.text label
                    prop.htmlFor id
                ] 
            ]

            Html.div [
                Html.input [
                    prop.placeholder label
                    prop.id id
                    prop.value field.Value
                    prop.onChange field.OnChange
                ]
            ]
        ]
    
    React.fragment [
        input "firstName"
        input "middleName"
        input "lastName"
    ]