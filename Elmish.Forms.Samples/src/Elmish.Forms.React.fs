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
        TouchField: FieldId -> unit
        GetIsTouched: FieldId -> bool
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
        TouchField: Browser.Types.FocusEvent -> unit
        IsTouched: bool
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
        TouchField = fun _ -> form.TouchField id
        IsTouched = form.GetIsTouched id
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
        Touched: Set<FieldId>
    }

type FormComponent<'Result, 'c>(props) as x=
    inherit Fable.React.Component<FormProps<'Result, unit, 'c>, FormState>(props)
    
    let updateValidation (model: FormState) validator =
        let result = Form.validate props.Validator () model.Model.FormFields
        let errors =
            match result with
            | Ok _ -> Map.empty
            | Error errors -> Map.ofList errors
        
        { model with Errors = errors }, result
    
    do
        x.setInitState(Unchecked.defaultof<_>)
        
    override x.componentDidMount() =
        x.setState(
            fun _ props ->
                { Model = Form.init props.Validator; Errors = Map.empty; Touched = Set.empty }
            )
        
    override x.render() =
        let model = x.state
        
        let updateField (id: FieldId) (value: string) =
            x.setState(
                fun model props ->
                    let newFormModel = Form.setField id (FieldState.String value) model.Model
                    let model, _ = updateValidation { model with Model = newFormModel } props.Validator
                    model
                )

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
                fun model props ->
                
                let model, result = updateValidation model props.Validator
                
                props.OnSubmit result
                model
            )
            
        let getIsRequired (id: FieldId) =
            Schema.getSchemaFromPath id model.Model
            |> Schema.getIsRequired
            
        let getErrorsMaybe (id: FieldId) =
            Map.tryFind id model.Errors
            
        let touchField (id: FieldId) = //TODO: Validate schema inclusion
            x.setState(
                fun model _ ->
                    { model with Touched = Set.add id model.Touched }
                )
        
        let getIsTouched (id: FieldId) =
            Set.contains id x.state.Touched
        
        let context: FormContext =
            {
                GetLabel = getLabel
                GetValue = getValue
                SetField = updateField
                FormSubmit = formSubmit
                GetIsRequired = getIsRequired
                GetErrors = getErrorsMaybe
                TouchField = touchField
                GetIsTouched = getIsTouched
            }

        let children =
            if model = Unchecked.defaultof<_> then React.fragment [] else props.Render()

        React.fragment
            [
                React.contextProvider (formContext, context, children)
            ]


let form<'Result, 'b, 'c> props =
    Fable.React.Helpers.ofType<FormComponent<'Result, 'c>, _, _> props []