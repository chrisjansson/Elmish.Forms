module Elmish.Forms.React

open Browser.Types
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
        NotShownErrors: Set<FieldId>
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
        let model: FormState =
            {
                Model = Form.init props.Validator
                Errors = Map.empty
                NotShownErrors = Set.empty
                IsSubmitted = false
            }
            |> updateValidation props.Validator
            |> fst
        
        x.setInitState(model)
    
    let mutable inputElement: Element option = None
    
    let updateShownErrors model =
        match inputElement with
        | Some el ->
            let elements = el.querySelectorAll("[data-validation-error-for]")
            let errorsWithShownIds =
                [
                    for i = 0 to (elements.length - 1) do
                        let e = elements.[i]
                        let a = e.attributes.getNamedItem("data-validation-error-for")
                        a.value
                ]
                |> Set.ofList
            
            let notShownErrors =
                model.Errors
                |> Map.filter (fun key _ -> not (Set.contains key errorsWithShownIds))
                |> Map.toList
                |> List.map fst
                |> Set.ofList
            
            { model with NotShownErrors = notShownErrors }
        | None ->
            { model with NotShownErrors = Set.empty }
        
    override x.componentDidMount() =
        x.setState(fun model _ -> updateShownErrors model)
    
    override x.componentDidUpdate(_, prevState) =
        let newModel = updateShownErrors prevState
        if prevState.Errors = newModel.Errors && prevState.NotShownErrors = newModel.NotShownErrors then
            ()
        else
            x.setState(
                fun x _ ->
                    if newModel.NotShownErrors.IsEmpty |> not then 
                        Fable.Core.JS.console.warn("Validation errors not shown: ", Set.toArray newModel.NotShownErrors)
                    { x with NotShownErrors = newModel.NotShownErrors }
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

        Html.div [
            prop.ref (fun e -> inputElement <- Option.ofObj e)
            prop.children [
                React.contextProvider (formContext, context, x.children)
            ]
        ]

let form<'Result, 'b, 'c> props children =
    Fable.React.Helpers.ofType<FormComponent<'Result, 'c>, _, _> props children

    
