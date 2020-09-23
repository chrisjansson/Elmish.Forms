module App

open System
open Browser.Dom
open Feliz

type SampleModel =
    {
        Submitted: Result<string, string list> option
    }

module SimpleSample =
    open Elmish.Forms

    let render = App.Samples.ApplicativeSample.render
    let validator = App.Samples.ApplicativeSample.validator
    let source = App.Samples.ApplicativeSample.source
    
    let renderForm =
        React.functionComponent (
            fun () ->
                let form = React.useForm()
                Html.form [
                    prop.onSubmit form.FormSubmit
                    prop.children [
                        render ()
                        
                        Html.div [ Html.button [ prop.type' "submit"; prop.text "Submit" ] ]
                    ]
                ]
            )

    let form =
        React.functionComponent(fun () ->
            let model, setModel = React.useState({ Submitted = None })
            
            let onSubmit (result: Result<_, ValidationErrors>) =
                let resultString =
                    match result with
                    | Ok r -> Fable.Core.JS.JSON.stringify(r) |> Ok
                    | Error e ->
                        e
                        |> List.collect (fun (id, list) -> list)
                        |> Error
                
                
                setModel(
                            { model with Submitted = Some resultString }
                        )
            
            React.fragment [
                React.form { Validator = validator; Render = renderForm; OnSubmit = onSubmit }
                if model.Submitted.IsSome then
                    match model.Submitted.Value with
                    | Ok r ->
                        Html.pre [
                            prop.style [
                                style.borderColor "green"
                                style.backgroundColor "rgb(221, 255, 216)"
                            ]
                            prop.children [
                                Html.text r
                            ]
                        ]
                    | Error r ->
                        Html.pre [
                            prop.style [
                                style.borderColor "red"
                                style.backgroundColor "#fff6f6"
                            ]
                            prop.children [
                                Html.unorderedList [
                                    for e in r do
                                        Html.li [ Html.text e ]
                                ]
                            ]
                  
                        ]
                        
                Html.pre [ Html.text source ]
            ]
        )
        



ReactDOM.render(SimpleSample.form, document.getElementById("root"))