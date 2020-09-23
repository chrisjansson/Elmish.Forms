module App.Samples.SimpleSample

open Elmish.Forms
open Feliz

let source: string = Fable.Core.JsInterop.importDefault (Utils.importLiteral + __SOURCE_FILE__)

let validator: Validator<string option, unit, unit> =
    Validator.Standard.text "firstName"
    |> Validator.withLabel "First name"
    
let render () =
    React.fragment [
        Gui.input "firstName"
    ]