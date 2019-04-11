module rec Forms

    module Model =  

        type FieldState = string
        type FieldId = string

        type ValidationError = string
        type KeyedValidationError = FieldId * ValidationError
        type ValidationResult<'a> = Result<'a, KeyedValidationError list>
        
        type Field =
            | Group of Group
            | List of Group list * Group
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

        type Msg =
            | InputChanged of FieldId * string
            | Touch of FieldId
            | Submit
            | AppendList of FieldId
            | RemoveListItem of FieldId * int

        let init (validator: Validator.Validator<'a>): Model<'a> =
            let (Validator.Validator { Default = d }) = validator
            let (Model.Group g) = d
            {
                Fields = g
                ValidationErrors = Map.empty
                Result = None    
                Touched = Set.empty
                IsSubmitted = false
            }

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
                    | (Path.List index, Model.List (l, _)) -> 
                        match List.tryItem index l with
                        | Some field -> find tail (Field.Group field)
                        | None -> None
                    | _ -> None
        
        let getFieldValue (id: FieldId) (model: Model<_>): FieldState = 
            let path = Path.parse id
            let field = model.Fields |> Model.Group

            match find path field with
            | Some (Leaf l) -> l
            | _ -> failwithf "Unknown path id %s" id
            
        let getListLength (id: FieldId) (model: Model<_>): int =
            let path = Path.parse id
            let field = model.Fields |> Model.Group

            match find path field with
            | Some (List (l, _)) -> List.length l
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
                | ([], Some (List (l, d))) ->
                    let newList = replacer (l, d)
                    List newList
                | (Path.List i ::rest, Some (List (l, d))) ->
                    let newList = List.mapi (fun index (node: Model.Group) ->
                            if index = i then
                                let (Field.Group g) = set rest (Some (Model.Field.Group node))
                                g
                            else
                                node) l
                    List <| (newList, d)
               
            let (Field.Group fields) = set path (Some (Field.Group model.Fields))
                
            { model with Fields = fields }
            
        let removeListItem (id: FieldId) (index: int) (model: Model<_>): Model<_> =
            let replacer (l, d) =
                let contents =
                    l
                    |> List.indexed
                    |> List.filter (fun (i, v) -> i <> index)
                    |> List.map snd
                (contents, d)
    
            replaceListNode id model replacer
            
        let appendListItem (id: FieldId) (model: Model<_>): Model<_> =
            let replacer (l, d) =
                (List.append l [ d ], d)
            
            replaceListNode id model replacer

        let setFieldValue (id: FieldId) (model: Model<_>) (fieldState: FieldState): Model<_> =
            let path = Path.parse id
            let rec set (path: Path.PathSegment list) (fields: Model.Field option) (field: FieldState) =
                match (path, fields) with
                | ([], Some (Leaf _)) -> Leaf fieldState
                | ( Path.Node n :: rest, Some (Group g)) ->
                    let node = Map.tryFind n g
                    let fs = set rest node field
                    let g = Map.add n fs g
                    Field.Group g
                | (Path.List i ::rest, Some (List (l, d))) ->
                    let updateListItem index item =
                        if index = i then
                                let (Field.Group g) = set rest (Some (Model.Field.Group item)) field
                                g
                            else
                                item
                    
                    let newList = List.mapi updateListItem l
                    List (newList, d)
                | (x, _) -> failwith (sprintf "uncaught path %A" x)
            
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
        
    
        type Validator<'T> = Validator of ValidatorT<'T>
        and ValidatorT<'T> =
            {
                F: (Context -> Model.Group -> Model.ValidationResult<'T>)
                Default:  Model.Field
                Context: Context
            }
        and Context =
            {
                Label: string
                Id: Model.FieldId
            }
        
        let from (f: 'T): Validator<'T> =
            Validator { F = (fun _ _ -> Ok f); Default = Model.Group Map.empty; Context = { Label = ""; Id = "" } }
            
        let traverse (v: List<Result<_, _>>) =
            let reducer left right =
                match left, right with
                | Ok l, Ok r -> Ok (l::r)
                | Ok _, Error e -> Error e
                | Error e, Ok _ -> Error e
                | Error l, Error r -> Error (l@r)
                
            List.foldBack reducer v (Ok [])
            
        let apply (vf: Validator<_>) (va: Validator<_>): Validator<_> =
            let (Validator { F = f; Default = df1; Context = c1 }) = vf
            let (Validator { F = a; Default = df2; Context = c2 }) = va
            
            let inner _ g =
                match (f c1 g, a c2 g) with
                | Ok rf, Ok ra -> Ok (rf ra)
                | Ok _, Error e -> Error e
                | Error e, Ok _ -> Error e
                | Error l, Error r -> Error <| l@r
                
            let joinedGroups =
                match (df1, df2) with
                | (Model.Group g1, Model.Group g2) ->
                    let s1 = Map.toSeq g1
                    let s2 = Map.toSeq g2
                    let seq = Seq.concat [ s1;s2 ]
                    Map.ofSeq seq
                | _ -> failwith "Invalid apply"
            
            Validator { F = inner; Default = Model.Group joinedGroups; Context = { Label = ""; Id = "" } }

        let withSub id (validator: Validator<_>): Validator<_> =
            let (Validator { F = v; Default = defaultValue; Context = c }) = validator
            let inner _ g =
                match Map.tryFind id g with
                | Some (Model.Group g) ->
                    match v c g with
                    | Ok r -> Ok r
                    | Error e ->
                        printfn "Group error"
                        let mappedErrors = List.map (fun (errorId, f) -> id + "." + errorId, f) e
                        Error mappedErrors
                | None ->
                    let defaultGroup = Map.empty
                    v c defaultGroup
                | _ -> Error [ (id, "Invalid group type")  ]
            
            let defaultValue =
                Model.Group <| Map.ofList [
                    id, defaultValue
                ]
                
            Validator { F = inner; Default = defaultValue; Context = { Label = ""; Id = id } }
            
        let withList id (validator: Validator<'a>): Validator<'a list> =
            let (Validator { F = v; Default = Model.Group d; Context = c }) = validator
            let inner _ g =
                match Map.tryFind id g with
                | Some (Model.List (l, _)) ->
                    let mapper index g =
                        match v c g with
                        | Ok r -> Ok r
                        | Error errors ->
                            let template eId = sprintf "%s.[%i].%s" id index eId
                            
                            let mappedErrors = List.map (fun (eId, m) -> (template eId, m)) errors
                            Error mappedErrors
                    List.mapi mapper l |> traverse
                | None ->
                    Ok []
                | _ -> Error [ (id, "Invalid group type")  ]
            Validator { F = inner; Default = Model.Group <| Map.ofList [ id, Model.List ([], d) ]; Context = { Label = ""; Id = id } }

        let text id =
            let inner _ (f: Model.Group) =
                match Map.tryFind id f with
                | Some (Model.Leaf v) ->
                    Ok <|
                        if System.String.IsNullOrWhiteSpace(v) then
                            None
                        else
                            Some v
                | None -> Ok None
                | _ -> Error [ (id, "Invalid group type")  ]
            Validator { F = inner; Default = Model.Group <| Map.ofList [ id, Model.Leaf "" ]; Context = { Label = id; Id = id } }

        let required (f: Validator<'a option>) =
            let (Validator vv) = f
            let { F = v; Default = d } = vv
            let inner context (g: Model.Group) =
                match (v context g) with
                | Ok (Some v) ->
                    Ok v
                | Ok None ->
                    Error [ (context.Id, sprintf "%s is required" context.Label) ]
                | Error e-> Error e
            Validator { F = inner; Default = d; Context = vv.Context }
            
        let withLabel label (f: Validator<_>) =
            let (Validator v) = f
            Validator { F = v.F; Default = v.Default; Context = { v.Context with Label = label }}

        let private tryParseInt (s: string) =
            match System.Int32.TryParse(s) with
            | true, i -> Some i
            | _ -> None
            
        let asInt (f: Validator<string option>) =
            let (Validator vv) = f
            let { F = v; Default = d } = vv
            let inner context (g: Model.Group) =
                match (v context g) with
                | Ok (Some v) ->
                    match tryParseInt v with
                    | Some i -> Ok <| Some i
                    | None -> Error  [ (context.Id, sprintf "%s must be an integer" context.Label) ]
                | Ok None -> Ok None
                | Error e-> Error e
            Validator { F = inner; Default = d; Context = vv.Context }
           
        let run (validator: Validator<'T>) (form: Model.Model<'T>) =
            let (Validator { F = v; Context = c }) = validator
            v c form.Fields
                       
    let (<*>) = Validator.apply