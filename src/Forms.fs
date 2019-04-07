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

        let rec private find (path: Path.PathSegment list) (field: Model.Field) =
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
        
        let getField (id: FieldId) (model: Model<_>): FieldState = 
            let path = Path.parse id
            let field = model.Fields |> Model.Group

            match find path field with
            | Some (Leaf l) -> l
            | _ -> Field.defaultValue
            
        let getListLength (id: FieldId) (model: Model<_>): int =
            let path = Path.parse id
            let field = model.Fields |> Model.Group

            match find path field with
            | Some (List l) -> List.length l
            | Some _ -> failwith "Wrong type in list length lookup"
            | _ -> 0
            
        let private replaceListNode (id: FieldId) (model: Model<_>) replacer: Model<_> =
            let path = Path.parse id
            let rec set (path: Path.PathSegment list) (fields: Model.Field option) =
                match (path, fields) with
                | ( Path.Node n :: rest, Some (Group g)) ->
                    let node = Map.tryFind n g
                    let fs = set rest node
                    let g = Map.add n fs g
                    Field.Group g
                | ([], Some (List l)) ->
                    let newList = replacer l
                    List newList
                | (Path.List i ::rest, Some (List l)) ->
                    let newList = List.mapi (fun index (node: Model.Group) ->
                            if index = i then
                                let (Field.Group g) = set rest (Some (Model.Field.Group node))
                                g
                            else
                                node) l
                    List newList
               
            let (Field.Group fields) = set path (Some (Field.Group model.Fields))
                
            { model with Fields = fields }
            
        let removeListItem (id: FieldId) (index: int) (model: Model<_>): Model<_> =
            let replacer l =
                l
                |> List.indexed
                |> List.filter (fun (i, v) -> i <> index)
                |> List.map snd
    
            replaceListNode id model replacer
            
        let appendListItem (id: FieldId) (model: Model<_>): Model<_> =
            let replacer l =
                List.append l [ Map.empty ]
            
            replaceListNode id model replacer

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
                    match v g with
                    | Ok r -> Ok r
                    | Error e ->
                        printfn "Group error"
                        let mappedErrors = List.map (fun (errorId, f) -> id + "." + errorId, f) e
                        Error mappedErrors
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
            
        let private tryParseInt (s: string) =
            match System.Int32.TryParse(s) with
            | true, i -> Some i
            | _ -> None
            
        let asInt id (f: Validator<string option>) =
            let inner (g: Model.Group) =
                let (Validator v) = f
                match (v g) with
                | Ok (Some v) ->
                    match tryParseInt v with
                    | Some i -> Ok <| Some i
                    | None -> Error  [ (id, (fun label -> (sprintf "%s must be an integer" label))) ]
                | Ok None -> Ok None
                | Error e-> Error e
            Validator inner
           
        let run (validator: Validator<'T>) (form: Model.Model<'T>) =
            let (Validator v) = validator
            v form.Fields
                       
    let (<*>) = Validator.apply