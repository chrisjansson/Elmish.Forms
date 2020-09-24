module App

open Browser.Dom
open Feliz

type SampleModel =
    {
        Submitted: Result<string, string list> option
    }

module SimpleSample =
    open Elmish.Forms

    type FormRenderProps =
        {
            Render: unit -> ReactElement
        }
    
    let renderForm =
        React.functionComponent (
            fun (props: FormRenderProps) ->
                let form = React.useForm()
                Html.form [
                    prop.onSubmit form.FormSubmit
                    prop.children [
                        props.Render ()
                        
                        Html.div [
                            Html.button [
                                prop.type' "submit"
                                prop.text "Submit"
                                prop.style [
                                    style.marginTop 10
                                ]
                            ]
                        ]
                    ]
                ]
            )

    type SampleDisplayProps<'a, 'b, 'c> =
        {
            Render: unit -> ReactElement
            Validator: Validator<'a, 'b, 'c>
            Source: string
        }
    
    let form () =
        React.functionComponent(fun (props: SampleDisplayProps<_, _, _>) ->
            let model, setModel = React.useState({ Submitted = None })
            
            let onSubmit (result: Result<_, ValidationErrors>) =
                let resultString =
                    match result with
                    | Ok r -> Fable.Core.JS.JSON.stringify(r) |> Ok
                    | Error e ->
                        e
                        |> List.collect (fun (_, list) -> list)
                        |> Error
                
                
                setModel({ model with Submitted = Some resultString })
            
            React.fragment [
                React.form { Validator = props.Validator; OnSubmit = onSubmit } [ renderForm { Render = props.Render } ]
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
                        
                Html.pre [ Html.text props.Source ]
            ]
        )
        
let samples =
    [
        SimpleSample.form () { Render = App.Samples.SimpleSample.render; Validator = App.Samples.SimpleSample.validator; SimpleSample.Source = App.Samples.SimpleSample.source }
        SimpleSample.form () { Render = App.Samples.ApplicativeSample.render; Validator = App.Samples.ApplicativeSample.validator; SimpleSample.Source = App.Samples.ApplicativeSample.source }
    ]
        
ReactDOM.render(React.fragment samples, document.getElementById("root"))