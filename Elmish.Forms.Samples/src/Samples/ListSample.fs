module App.Samples.List


open App.Samples.ApplicativeSample
open Elmish.Forms
open Feliz

let source: string = Fable.Core.JsInterop.importDefault (Utils.importLiteral + __SOURCE_FILE__)

let validator: Validator<Person list, unit, unit> =
    Validator.withList "persons" (ApplicativeSample.validator ())
    
let render () =
    React.fragment [
        
        let form = React.useForm()
        
        Html.button [
            prop.text "Add row"
            prop.onClick (
                fun e ->
                    e.preventDefault()
                    form.AddListItem "persons"
                )
        ]

        let listLength = form.GetListLength "persons"
        Html.text listLength
        
        for p = 0 to (listLength - 1) do
            
            Html.button [
                prop.text "Remove row"
                prop.onClick (
                    fun e ->
                        e.preventDefault()
                        form.RemoveListItem "persons" p
                    )
            ]
            
            Gui.input (sprintf "persons[%i].firstName" p)
            Gui.input (sprintf "persons[%i].middleName" p)
            Gui.input (sprintf "persons[%i].lastName" p)
    ]