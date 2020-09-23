module App

open System
open Browser.Dom
open Feliz

type SampleModel =
    {
        Submitted: string option
    }

module SimpleSample =
    open Elmish.Forms

    let model: SampleModel =
        {
            Submitted = None
        }
        
    let render = App.Samples.ApplicativeSample.render
    let validator = App.Samples.ApplicativeSample.validator
    
    let renderForm =
        React.functionComponent (
            fun () ->
                let form = React.useForm()
                Html.form [
                    prop.onSubmit form.FormSubmit
                    prop.children [
                        render ()
                        
                        Html.div [ Html.button [ prop.type' "submit"; prop.text "Submit" ] ]
                        
                        if Option.isSome model.Submitted then
                            Html.pre [ prop.text model.Submitted.Value ]
                    ]

                ]
            )

    let form =
        React.functionComponent(fun () ->
            let model, setModel = React.useState({ Submitted = None })
            
            let onSubmit result =
                setModel({ model with Submitted = Some (Fable.Core.JS.JSON.stringify(result)) })
            
            React.fragment [
                React.form { Validator = validator; Render = renderForm; OnSubmit = onSubmit }
                if model.Submitted.IsSome then
                    Html.pre [ Html.text model.Submitted.Value ]
            ]
        )
        



ReactDOM.render(SimpleSample.form, document.getElementById("root"))