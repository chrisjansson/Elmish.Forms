module App.SimpleSample

open Elmish.Forms
open Feliz
   
type UseFormData =
    {
        GetLabel: FieldId -> string
        GetValue: FieldId -> string
        SetField: FieldId -> string -> unit
        FormSubmit: Browser.Types.Event -> unit
    }
    
type ElmishFormData<'a, 'b, 'c> =
    {
        Validator: Validator<'a, 'b, 'c>
    }
    
let formContext = React.createContext<UseFormData>()

let useForm () =
    React.useContext(formContext)
    
type FieldData =
    {
        Value: string
        OnChange: Browser.Types.Event -> unit
        Label: string
    }

open Fable.Core.JsInterop

let useField (id: FieldId) =
    let form = useForm()
    
    let value = form.GetValue id
    let onChange (event: Browser.Types.Event) =
        let value = event.currentTarget?value
        
        form.SetField id value
    
    {
        Value = value
        OnChange = onChange
        Label = form.GetLabel id
    }

type FormProps<'Result, 'b, 'c> =
    {
        Validator: Validator<'Result, 'b, 'c>
        Render: unit -> ReactElement
        OnSubmit: Result<'Result, ValidationErrors> -> unit
    }
    
let form<'Result, 'b, 'c> =
    React.functionComponent (
        fun (props: FormProps<'Result, unit, 'c>) ->
            let model, setModel = React.useState(Unchecked.defaultof<Model>)

            React.useEffectOnce(fun () ->
                let state = Form.init props.Validator
                setModel(state)
            )
                        
            let updateField (id: FieldId) (value: string) =
                let formModel = Form.setField id (FieldState.String value) model
                setModel(formModel)
            
            let getValue (id: FieldId) =
                match Form.getField id model with
                | FieldState.String s -> s

            let getLabel (id: FieldId) =
                Schema.getSchemaFromPath id model
                |> Schema.getLabel
                |> Option.defaultValue ""
        
            let formSubmit (e: Browser.Types.Event) =
                e.preventDefault()
                let result = Form.validate props.Validator () model.FormFields
                props.OnSubmit result
                        
            let context: UseFormData =
                {
                    GetLabel = getLabel
                    GetValue = getValue
                    SetField = updateField
                    FormSubmit = formSubmit 
                }
            
            let children =
                if model = Unchecked.defaultof<_> then
                    React.fragment []
                else
                    props.Render ()
            
            React.fragment [
                React.contextProvider(formContext, context, children)
            ]
        )

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