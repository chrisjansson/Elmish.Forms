module Elmish.Forms.React

open Elmish.Forms
open Feliz
open Fable.Core.JsInterop

type FormContext =
    {
        GetLabel: FieldId -> string
        GetValue: FieldId -> string
        SetField: FieldId -> string -> unit
        FormSubmit: Browser.Types.Event -> unit
    }
    
type Field =
    {
        Value: string
        OnChange: Browser.Types.Event -> unit
        Label: string
    }

type ElmishFormData<'a, 'b, 'c> = { Validator: Validator<'a, 'b, 'c> }

let formContext = React.createContext<FormContext> ()

let useForm () = React.useContext (formContext)

let useField (id: FieldId) =
    let form = useForm ()

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
    React.functionComponent (fun (props: FormProps<'Result, unit, 'c>) ->
        let model, setModel =
            React.useState (Unchecked.defaultof<Model>)

        React.useEffectOnce (fun () ->
            let state = Form.init props.Validator
            setModel (state))

        let updateField (id: FieldId) (value: string) =
            let formModel =
                Form.setField id (FieldState.String value) model

            setModel (formModel)

        let getValue (id: FieldId) =
            match Form.getField id model with
            | FieldState.String s -> s

        let getLabel (id: FieldId) =
            Schema.getSchemaFromPath id model
            |> Schema.getLabel
            |> Option.defaultValue ""

        let formSubmit (e: Browser.Types.Event) =
            e.preventDefault ()

            let result =
                Form.validate props.Validator () model.FormFields

            props.OnSubmit result

        let context: FormContext =
            {
                GetLabel = getLabel
                GetValue = getValue
                SetField = updateField
                FormSubmit = formSubmit
            }

        let children =
            if model = Unchecked.defaultof<_> then React.fragment [] else props.Render()

        React.fragment
            [
                React.contextProvider (formContext, context, children)
            ])
