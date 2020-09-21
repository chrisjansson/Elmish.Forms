module App

open Browser.Dom
open Fable.Core.JsInterop

module SimpleSample =
    open Elmish.Forms
    
    let validator: Validator<string option, unit, unit> =
        Validator.Standard.text "firstName"
        |> Validator.withLabel "First name"
        
    open Feliz
    
    type SampleFormModel =
        {
            FormModel: Model
            Submitted: string option
        }
        
    let model: SampleFormModel =
        {
            FormModel = Form.init validator
            Submitted = None
        }
    
    let form =
        React.functionComponent(fun () ->
            let model, setModel = React.useState(model)
            
            let updateField (id: FieldId) (value: string) =
                let formModel = Form.setField id (FieldState.String value) model.FormModel
                setModel({ model with FormModel = formModel })
            
            let getValue (id: FieldId) =
                match Form.getField id model.FormModel with
                | FieldState.String s -> s
                | _ -> failwith "Unknown field state type"
                
            let getLabel (id: FieldId) =
                Schema.getSchemaFromPath id model.FormModel
                |> Schema.getLabel
                |> Option.defaultValue ""
            
            Html.div [
                Html.form [
                    prop.onSubmit (
                                    fun e ->
                                        e.preventDefault()
                                        
                                        let value =
                                            Form.validate validator () model.FormModel.FormFields
                                        
                                        setModel({ model with Submitted = Some (Fable.Core.JS.JSON.stringify(value)) })
                                  )
                    prop.children [
                        let label = getLabel "firstName"

                        Html.div [
                            
                            Html.label [
                                prop.text label
                                prop.htmlFor "firstName"
                            ] 
                        ]
                        Html.div [
                            Html.input [
                                prop.id "firstName"
                                prop.onInput (fun e -> updateField "firstName" e.currentTarget?value)
                                prop.placeholder label
                            ]

                        ]
                        
                        Html.div [
                            Html.input [ prop.type' "submit" ]
                            
                        ]
                        
                        if Option.isSome model.Submitted then
                            Html.code [
                                prop.text model.Submitted.Value
                            ]
                    ]

                ]
            ]
        )
        

open Feliz
open Browser.Dom

ReactDOM.render(SimpleSample.form, document.getElementById("root"))