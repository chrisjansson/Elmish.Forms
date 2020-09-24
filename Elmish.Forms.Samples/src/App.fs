module App

open Browser.Dom
open Feliz

type SampleModel =
    {
        Submitted: Result<string, string list> option
    }

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
        "Simple validator", form () { Render = App.Samples.SimpleSample.render; Validator = App.Samples.SimpleSample.validator; Source = App.Samples.SimpleSample.source }
        "Applicative validator", form () { Render = App.Samples.ApplicativeSample.render; Validator = App.Samples.ApplicativeSample.validator; Source = App.Samples.ApplicativeSample.source }
    ]
        
let sampleApp =
    React.functionComponent (fun () ->
            let selectedSampleIndex, setSelectedSample = React.useState(0)
            let indexedSamples = samples |> List.indexed
            let selectedSample =
                indexedSamples
                |> List.find (fun (index, _) -> index = selectedSampleIndex)
                |> snd
                |> snd
            
            Html.div [
                prop.style [
                    style.display.flex
                    style.flexDirection.row
                ]
                prop.children [
                    Html.unorderedList [
                        for index, sample in indexedSamples do
                            Html.li [
                                prop.style [
                                    
                                    style.listStyleType.none
                                    if index = selectedSampleIndex then
                                        style.textDecoration.underline
                                ]
                                prop.children [
                                    Html.a [
                                        prop.style [
                                            
                                        ]
                                        prop.text (fst sample)
                                        prop.onClick (
                                            fun e ->
                                                e.preventDefault()
                                                setSelectedSample(index)
                                                     )
                                    ]
                                ]

                            ]
                    ]
                    
                    Html.div [
                        prop.style [
                            style.marginLeft 10
                        ]
                        prop.children [
                            selectedSample
                        ]
                    ]
                ]
                

            ]

        )
        
ReactDOM.render(sampleApp, document.getElementById("root"))