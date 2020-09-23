module Elmish.Forms.React

open Elmish.Forms
open Feliz
open Fable.Core.JsInterop

type FormContext =
    {
        GetIsRequired: FieldId -> bool 
        GetLabel: FieldId -> string
        GetValue: FieldId -> string
        SetField: FieldId -> string -> unit
        FormSubmit: Browser.Types.Event -> unit
        GetErrors: FieldId -> (string list) option
    }
    
type Field =
    {
        Value: string
        OnChange: Browser.Types.Event -> unit
        Label: string
        IsRequired: bool
        ErrorMessage: (string list) option
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
        IsRequired = form.GetIsRequired id
        ErrorMessage = form.GetErrors id
    }

type FormProps<'Result, 'b, 'c> =
    {
        Validator: Validator<'Result, 'b, 'c>
        Render: unit -> ReactElement
        OnSubmit: Result<'Result, ValidationErrors> -> unit
    }
    
type FormState =
    {
        Model: Model
        Errors: Map<FieldId, string list>
    }

type FormComponent<'Result, 'c>(props) as x=
    inherit Fable.React.Component<FormProps<'Result, unit, 'c>, FormState>(props)
    
    do
        x.setInitState(Unchecked.defaultof<_>)
        
    override x.componentDidMount() =
        x.setState(
            fun _ props ->
                { Model = Form.init props.Validator; Errors = Map.empty }
            )
        
    override x.render() =
        let model = x.state
        
        let updateField (id: FieldId) (value: string) =
            x.setState(fun model _ -> { model with Model = Form.setField id (FieldState.String value) model.Model })

        let getValue (id: FieldId) =
            match Form.getField id model.Model with
            | FieldState.String s -> s

        let getLabel (id: FieldId) =
            Schema.getSchemaFromPath id model.Model
            |> Schema.getLabel
            |> Option.defaultValue ""

        let formSubmit (e: Browser.Types.Event) =
            e.preventDefault ()
            x.setState(
                fun model _ ->
                         
                let result =
                    Form.validate props.Validator () model.Model.FormFields

                let errors =
                    match result with
                    | Ok _ -> Map.empty
                    | Error errors -> Map.ofList errors
            
                props.OnSubmit result
                { model with Errors = errors }
            )
            

        let getIsRequired (id: FieldId) =
            Schema.getSchemaFromPath id model.Model
            |> Schema.getIsRequired
            
        let getErrorsMaybe (id: FieldId) =
            Map.tryFind id model.Errors
        
        let context: FormContext =
            {
                GetLabel = getLabel
                GetValue = getValue
                SetField = updateField
                FormSubmit = formSubmit
                GetIsRequired = getIsRequired
                GetErrors = getErrorsMaybe
            }

        let children =
            if model = Unchecked.defaultof<_> then React.fragment [] else props.Render()

        React.fragment
            [
                React.contextProvider (formContext, context, children)
            ]


let form<'Result, 'b, 'c> props =
    Fable.React.Helpers.ofType<FormComponent<'Result, 'c>, _, _> props []