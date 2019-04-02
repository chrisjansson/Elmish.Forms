module Forms

    module Model =  

        type FieldState = string
        type FieldName = string
        type FieldId = string

        type ValidationError = string -> string
        type KeyedValidationError = FieldId * ValidationError
        type ValidationResult<'a> = Result<'a, KeyedValidationError list>
        

        type Field =
            | Group of Group
            | List of Group list
            | Leaf of FieldState
        and Group = Map<FieldId, Field>        

        type Model<'T> =
            {
                Fields: Group
                ValidationErrors: Map<FieldId, ValidationError list>
                Touched: Set<FieldId>
                Result: 'T option
                IsSubmitted: bool
            }

        type Validator<'a, 'b> = (Model<'b> -> ValidationResult<'a>)

        type FieldDefinition<'a, 'b> = 
            { 
                Id: FieldId
                Name: string
                Validate: Validator<'a, 'b>
            }

        type Msg =
            | InputChanged of FieldId * string
            | Touch of FieldId
            | Submit

        let init() : Model<'a> = {
            Fields = Map.empty
            ValidationErrors = Map.empty
            Result = None    
            Touched = Set.empty
            IsSubmitted = false
        }
        
    module FieldId =
        open Model

        let create (id: string): FieldId = id

    module Field =
        open Model

        let defaultValue: FieldState = ""

    module Form =
        open Model
        open Extensions          
        
        let getField (id: FieldId) (model: Model<_>): FieldState = 
            Map.tryFind id model.Fields 
            |> Option.map (fun (Leaf l) -> l)
            |> Option.defaultValue Field.defaultValue

        let getField2 (id: FieldId) (model: Model<_>): FieldState = 
            let rec find (path: Path.PathSegment list) (field: Model.Field) =
                match (path, field) with
                | ([], field) -> Some field
                | (head::tail, field) ->
                    match (head, field) with
                    | (Path.Node n, Model.Group g) ->
                        match Map.tryFind n g with
                        | Some field -> find tail field
                        | None -> None
                    | (Path.List index, Model.List l) -> 
                        match List.tryItem index l with
                        | Some field -> find tail (Field.Group field)
                        | None -> None
                    | _ -> None

            let path = Path.parse id
            let field = model.Fields |> Model.Group

            match find path field with
            | Some (Leaf l) -> l
            | _ -> Field.defaultValue

        let setField (id: FieldId) (model: Model<_>) (fieldState: FieldState): Model<_> =
            let path = Path.parse id
            let rec set (path: Path.PathSegment list) (fields: Model.Field option) (field: FieldState) =
                match (path, fields) with
                | ([], Some (Leaf fs)) -> Leaf fieldState
                //Insert when leaf node does not exist
                | ([], None) -> Leaf fieldState
                | ( Path.Node n :: rest, Some (Group g)) ->
                    let node = Map.tryFind n g
                    let fs = set rest node field
                    let g = Map.add n fs g
                    Field.Group g
                //Insert when group node does not exist
                | ( Path.Node n :: rest, None) ->
                    let fs = set rest None field
                    let g = Map.add n fs Map.empty
                    Field.Group g
                | (Path.List i ::rest, Some (List l)) ->
                    let newList = List.mapi (fun index (node: Model.Group) ->
                            if index = i then
                                let (Field.Group g) = set rest (Some (Model.Field.Group node)) field
                                g
                            else
                                node) l
                    List newList
//                | (x, _) -> failwith (sprintf "uncaught path %A" x)
            
            let (Field.Group fields) = set path (Some (Field.Group model.Fields)) fieldState
                
            { model with Fields = fields }

        let getValidationErrors (id: FieldId) (model: Model<_>): ValidationError list =
            Map.tryFind id model.ValidationErrors
            |> Option.defaultValue []

        let hasValidationError (id: FieldId) (model: Model<_>): bool =
            getValidationErrors id model |> List.isEmpty |> not

        let isTouched (id: FieldId) (model: Model<_>): bool =
            Set.contains id model.Touched

        let map2 f a b (model: Model<_>) =
            let fa = Result.lift2 f
            let a' = a model
            let b' = b model
            fa a' b'

        let map3 f a b c (model: Model<_>) =
            let fa = Result.lift3 f
            let a' = a model
            let b' = b model
            let c' = c model
            fa a' b' c'

        let map4 f a b c d (model: Model<_>) =
            let fa = Result.lift4 f
            let a' = a model
            let b' = b model
            let c' = c model
            let d' = d model
            fa a' b' c' d'
            
    
    module Validator =
        type Validator<'T> = Validator of (Model.Group -> Model.ValidationResult<'T>)
        
        let from (f: 'T): Validator<'T> =
            Validator (fun _ -> Ok f)
            
        let traverse (v: List<Result<_, _>>) =
            let reducer left right =
                match left, right with
                | Ok l, Ok r -> Ok (l::r)
                | Ok _, Error e -> Error e
                | Error e, Ok _ -> Error e
                | Error l, Error r -> Error (l@r)
                
            List.foldBack reducer v (Ok [])
            
        let apply (vf: Validator<_>) (va: Validator<_>): Validator<_> =
            let inner g =
                let (Validator f) = vf
                let (Validator a) = va
                
                match (f g, a g) with
                | Ok rf, Ok ra -> Ok (rf ra)
                | Ok _, Error e -> Error e
                | Error e, Ok _ -> Error e
                | Error l, Error r -> Error <| l@r
            Validator inner

        let withSub id (validator: Validator<_>): Validator<_> =
            let inner g =
                let (Validator v) = validator
                match Map.tryFind id g with
                | Some (Model.Group g) ->
                    //TODO: Map error ids
                    v g
                | None ->
                    let defaultGroup = Map.empty
                    v defaultGroup
                | _ -> Error [ (id, (fun _ -> "Invalid group type"))  ]
            Validator inner
            
        let withList id (validator: Validator<'a>): Validator<'a list> =
            let inner g =
                let (Validator v) = validator
                match Map.tryFind id g with
                | Some (Model.List l) ->
                    //TODO: Map error ids
                    let mapper g =
                        v g
                    List.map mapper l |> traverse
                | None ->
                    Ok []
                | _ -> Error [ (id, (fun _ -> "Invalid group type"))  ]
            Validator inner

        
        let text id =
            let inner (f: Model.Group) =
                match Map.tryFind id f with
                | Some (Model.Leaf v) ->
                    Ok <|
                        if System.String.IsNullOrWhiteSpace(v) then
                            None
                        else
                            Some v
                | None -> Ok None
                | _ -> Error [ (id, (fun _ -> "Invalid group type"))  ]
            Validator inner
            
        let required id (f: Validator<'a option>) =
            let inner (g: Model.Group) =
                let (Validator v) = f
                match (v g) with
                | Ok (Some v) ->
                    Ok v
                | Ok None ->
                    Error [ (id, (fun label -> (sprintf "%s is required" label))) ]
                | Error e-> Error e
            Validator inner
            
           
        let run (validator: Validator<'T>) (form: Model.Model<'T>) =
            let (Validator v) = validator
            v form.Fields
                       
    let (<*>) = Validator.apply
//                       
//    module FieldDefinition =
//        open Model
//        let private createDefaultOptionalFieldValidator (id: FieldId) =
//            fun model -> 
//                let field = Form.getField id model
//                if field.Length = 0 then
//                    None |> Ok
//                else    
//                    Some field |> Ok
//
//        let define (id: string): FieldDefinition<string option, _> =
//            let fieldId: FieldId = FieldId.create id
//            {
//                Id = fieldId
//                Name = id
//                Validate = createDefaultOptionalFieldValidator fieldId
//            }
//
//        let withName (name: FieldName) (field: FieldDefinition<_, _>)  =
//            { field with Name = name }
//
//        let isRequired (field: FieldDefinition<'a option, _>) =
//            let requiredValidation model =
//                match field.Validate model with
//                | Ok (Some v) -> Ok v
//                | Ok None -> 
//                    let requiredErrorMessage = sprintf "%s is required" field.Name
//                    Error [ (field.Id, requiredErrorMessage) ]
//                | Error e -> Error e
//            { 
//                Id = field.Id
//                Name = field.Name
//                Validate = requiredValidation 
//            }        
//
//        let minimumLength (length: int) (field: FieldDefinition<string option, _>) =
//            let lengthValidation model =
//                match field.Validate model with
//                | Ok (Some v) -> 
//                    if v.Length >= length then
//                        v |> Some |> Ok
//                    else                 
//                        let errorMessage = sprintf "%s must be at least %i characters" field.Name length
//                        Error [ (field.Id, errorMessage) ]
//                | Ok None -> Ok None                
//                | Error e -> Error e            
//            {
//                Id = field.Id
//                Name = field.Name
//                Validate = lengthValidation
//            }        
//
//        let int (field: FieldDefinition<string option, _>) =
//            let intValidation model =
//                match field.Validate model with
//                | Ok (Some v) -> 
//                    match System.Int32.TryParse(v) with
//                    | (true, i) -> i |> Some |> Ok
//                    | _ -> 
//                        let errorMessage = sprintf "%s must be an integer" field.Name
//                        Error [ (field.Id, errorMessage) ]
//                | Ok None -> Ok None                
//                | Error e -> Error e            
//            {
//                Id = field.Id
//                Name = field.Name
//                Validate = intValidation
//            }        
//
//        let greaterThan (limit: int) (field: FieldDefinition<int option, _>) =
//            let limitValidation model =
//                match field.Validate model with
//                | Ok (Some v) -> 
//                    if v >= limit then
//                        v |> Some |> Ok
//                    else                 
//                        let errorMessage = sprintf "%s must be at greater than %i" field.Name limit
//                        Error [ (field.Id, errorMessage) ]
//                | Ok None -> Ok None                
//                | Error e -> Error e            
//            {
//                Id = field.Id
//                Name = field.Name
//                Validate = limitValidation
//            }        
//
//        let predicate (pred: 'a -> bool) (buildMessage: string -> string) (field: FieldDefinition<'a option, _>) =
//            let validate model =
//                match field.Validate model with
//                | Ok (Some v) ->
//                    if pred v then
//                        v |> Some |> Ok
//                    else
//                        let errorMessage = buildMessage field.Name
//                        Error [ (field.Id, errorMessage) ]                
//                | Ok None -> Ok None                
//                | Error e -> Error e                
//            {
//                Id = field.Id
//                Name = field.Name
//                Validate = validate
//            }        
