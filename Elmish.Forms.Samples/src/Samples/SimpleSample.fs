module App.Samples.SimpleSample

open Elmish.Forms
open Elmish.Forms.React
open Feliz

let source: string = Fable.Core.JsInterop.importDefault (Utils.importLiteral + __SOURCE_FILE__)

let validator: Validator<string option, unit, unit> =
    Validator.Standard.text "firstName"
    |> Validator.withLabel "First name"
    
let render () =
    let id = "firstName"
    let field = useField id
    
    React.fragment [
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