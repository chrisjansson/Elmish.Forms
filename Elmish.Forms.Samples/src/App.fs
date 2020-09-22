module App

open Browser.Dom
open Fable.Core.JsInterop

type SampleModel =
    {
        Submitted: string option
    }

module SimpleSample =
    open Elmish.Forms
    
    let validator: Validator<string option, unit, unit> =
        Validator.Standard.text "firstName"
        |> Validator.withLabel "First name"
        
    open Feliz
    
    let model: SampleModel =
        {
            Submitted = None
        }
        
    
    let form =
        React.functionComponent(fun () ->
            let model, setModel = React.useState(model)
            
            let onSubmit result =
                setModel({ model with Submitted = Some (Fable.Core.JS.JSON.stringify(result)) })

            let render =
                React.functionComponent (
                    fun () ->
                        let form = Elmish.Forms.React.useForm()
                        Html.form [
                            prop.onSubmit form.FormSubmit
                            prop.children [
                                App.SimpleSample.render ()
                                
                                Html.div [ Html.button [ prop.type' "submit"; prop.text "Submit" ] ]
                                
                                if Option.isSome model.Submitted then
                                    Html.pre [ prop.text model.Submitted.Value ]
                            ]

                        ]
                    )
            
            Elmish.Forms.React.form { Validator = App.SimpleSample.validator; Render = render; OnSubmit = onSubmit }
            
            
        )
        

open Feliz
open Browser.Dom

ReactDOM.render(SimpleSample.form, document.getElementById("root"))