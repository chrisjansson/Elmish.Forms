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
        AddListItem: FieldId -> unit
        RemoveListItem: FieldId -> int -> unit
        GetListLength: FieldId -> int
    }
and ListContext =
    {
        AddItem: unit -> unit
        RemoveItem: int -> unit
        Length: int
        RenderItems: ((unit -> unit) -> Fable.React.ReactElement) -> Fable.React.ReactElement
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

type PrefixContext =
    {
        Prefix: FieldId
    }

let formContext = React.createContext<FormContext> ()

let prefixContext = React.createContext<PrefixContext>(defaultValue = { Prefix = "" })

let useForm () =
    React.useContext (formContext)

let usePrefix () =
    React.useContext(prefixContext).Prefix

let prefixContextProvider (prefix: FieldId) (children: Fable.React.ReactElement) =
    React.contextProvider(prefixContext, { Prefix = prefix }, children)

let useField (id: FieldId) =
    let form = useForm ()
    let prefix = usePrefix()
    let id = prefix + id
    
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
    
    
type ListPrefixProps =
    {
        Id: FieldId
        Render: (unit -> unit) -> Fable.React.ReactElement
    }
    
let withListPrefix' =
    React.functionComponent (fun (props: ListPrefixProps) ->
        let id = props.Id
        let render = props.Render
        
        let prefix = usePrefix ()
        let prefixedId =
            if prefix = "" then
                id
            else
                sprintf "%s.%s" prefix id
                
        printfn "%A" prefixedId
        let form = useForm ()
        let listLength = form.GetListLength prefixedId
        
        React.fragment [
            for i = 0 to (listLength - 1) do
                
                let removeItem () =
                    form.RemoveListItem prefixedId i
                
                prefixContextProvider (sprintf "%s[%i]." prefixedId i) (render removeItem)
        ]
    )
    
let withListPrefix (id: FieldId) (render: _ -> Fable.React.ReactElement) = withListPrefix' { Id = id; Render = render }
    
    
let useList (id: FieldId): ListContext =
    let form = useForm ()
    let prefix = usePrefix()
    let id = prefix + id
    
    {
        AddItem = fun () -> form.AddListItem id
        RemoveItem = fun index -> form.RemoveListItem id index
        Length = form.GetListLength id
        RenderItems = fun render -> withListPrefix id render
    }


type FormProps<'Result, 'b, 'c> =
    {
        Validator: Validator<'Result, 'b, 'c>
        OnSubmit: Result<'Result, ValidationErrors> -> unit
    }
    
type FormState =
    {
        Model: Model
        Errors: Map<FieldId, string list>
        IsSubmitted: bool
    }

type FormComponent<'Result, 'c>(props) as x=
    inherit Fable.React.Component<FormProps<'Result, unit, 'c>, FormState>(props)
    
    let updateValidation validator (model: FormState) =
        let result = Form.validate validator () model.Model.FormFields
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
                {
                    Model = Form.init props.Validator
                    Errors = Map.empty
                    IsSubmitted = false
                }
                |> updateValidation props.Validator
                |> fst
            )
        
    override x.render() =
        let model = x.state
        
        let updateField (id: FieldId) (value: string) =
            x.setState(
                fun model props ->
                    let newFormModel = Form.setField id value model.Model
                    let model, _ = updateValidation props.Validator { model with Model = newFormModel } 
                    model
                )

        let getValue (id: FieldId) =
            match Form.getField id model.Model with
            | FieldState.String (s, _) -> s

        let getLabel (id: FieldId) =
            Schema.getSchemaFromPath id model.Model
            |> Schema.getLabel
            |> Option.defaultValue ""

        let formSubmit (e: Browser.Types.Event) =
            e.preventDefault ()
            x.setState(
                fun model props ->
                
                let model, result = updateValidation props.Validator model 
                let model = { model with IsSubmitted = true }
                
                props.OnSubmit result
                model
            )
            
        let getIsRequired (id: FieldId) =
            Schema.getSchemaFromPath id model.Model
            |> Schema.getIsRequired
            
        let getErrorsMaybe (id: FieldId) =
            Map.tryFind id model.Errors
            
        let touchField (id: FieldId) =
            x.setState(
                fun model _ ->
                    { model with Model = Form.setTouched id model.Model }
                )
        
        let getIsTouched (id: FieldId) =
            if model.IsSubmitted then
                true
            else
                match Form.getField id model.Model with
                | FieldState.String (_, data) -> data.IsTouched
        
        let addListItem (id: FieldId) =
            x.setState(fun model _ -> { model with Model = Form.addListItem id model.Model })
        
        let removeListItem (id: FieldId) index =
            x.setState(fun model _ -> { model with Model = Form.removeListItem id index model.Model })
        
        
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
                AddListItem = addListItem
                RemoveListItem = removeListItem
                GetListLength = fun id -> Form.getListLength id model.Model
            }

        let children =
            if model = Unchecked.defaultof<_> then [|  |] else x.children

        React.fragment
            [
                React.contextProvider (formContext, context, children)
            ]

let form<'Result, 'b, 'c> props children =
    Fable.React.Helpers.ofType<FormComponent<'Result, 'c>, _, _> props children

    
