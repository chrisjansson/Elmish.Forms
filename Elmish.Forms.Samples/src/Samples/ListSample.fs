module App.Samples.List


open App.Samples.ApplicativeSample
open Elmish.Forms
open Feliz

let source: string = Fable.Core.JsInterop.importDefault (Utils.importLiteral + __SOURCE_FILE__)

let validator: Validator<Person list, unit, unit> =
    Validator.withList "persons" (ApplicativeSample.validator ())
    
let render () =
    React.fragment [
        let persons = React.useList "persons"
        
        Html.div [
            Html.text persons.Length
        ]
        
        Html.button [
            prop.text "Add row"
            prop.onClick (
                fun e ->
                    e.preventDefault()
                    persons.AddItem ()
                )
        ]

        persons.RenderItems (
            fun removeItem ->
                Html.div [
                    prop.style [
                        style.display.flex
                        style.flexDirection.row
                    ]
                    prop.children [
                        Gui.input "firstName"
                        Gui.input "middleName"
                        Gui.input "lastName"
                
                        Html.button [
                            prop.text "Remove row"
                            prop.onClick (
                                fun e ->
                                    e.preventDefault()
                                    removeItem ()
                                )
                        ]
                ]
            ]
        )
    ]