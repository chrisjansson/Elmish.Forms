module Forms

    module Model =  

        type FieldState = string
        type FieldName = string
        type FieldId = string

        type ValidationError = string
        type KeyedValidationError = FieldId * ValidationError
        type ValidationResult<'a> = Result<'a, KeyedValidationError list>

        type Field =
            | Group of Map<FieldId, Field>
            //| List of FieldS list
            | Leaf of FieldState

        type Model<'T> =
            {
                Fields: Map<FieldId, FieldState>
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
            |> Option.defaultValue Field.defaultValue

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

    module FieldDefinition =
        open Model
        let private createDefaultOptionalFieldValidator (id: FieldId) =
            fun model -> 
                let field = Form.getField id model
                if field.Length = 0 then
                    None |> Ok
                else    
                    Some field |> Ok

        let define (id: string): FieldDefinition<string option, _> =
            let fieldId: FieldId = FieldId.create id
            {
                Id = fieldId
                Name = id
                Validate = createDefaultOptionalFieldValidator fieldId
            }

        let withName (name: FieldName) (field: FieldDefinition<_, _>)  =
            { field with Name = name }

        let isRequired (field: FieldDefinition<'a option, _>) =
            let requiredValidation model =
                match field.Validate model with
                | Ok (Some v) -> Ok v
                | Ok None -> 
                    let requiredErrorMessage = sprintf "%s is required" field.Name
                    Error [ (field.Id, requiredErrorMessage) ]
                | Error e -> Error e
            { 
                Id = field.Id
                Name = field.Name
                Validate = requiredValidation 
            }        

        let minimumLength (length: int) (field: FieldDefinition<string option, _>) =
            let lengthValidation model =
                match field.Validate model with
                | Ok (Some v) -> 
                    if v.Length >= length then
                        v |> Some |> Ok
                    else                 
                        let errorMessage = sprintf "%s must be at least %i characters" field.Name length
                        Error [ (field.Id, errorMessage) ]
                | Ok None -> Ok None                
                | Error e -> Error e            
            {
                Id = field.Id
                Name = field.Name
                Validate = lengthValidation
            }        

        let int (field: FieldDefinition<string option, _>) =
            let intValidation model =
                match field.Validate model with
                | Ok (Some v) -> 
                    match System.Int32.TryParse(v) with
                    | (true, i) -> i |> Some |> Ok
                    | _ -> 
                        let errorMessage = sprintf "%s must be an integer" field.Name
                        Error [ (field.Id, errorMessage) ]
                | Ok None -> Ok None                
                | Error e -> Error e            
            {
                Id = field.Id
                Name = field.Name
                Validate = intValidation
            }        

        let greaterThan (limit: int) (field: FieldDefinition<int option, _>) =
            let limitValidation model =
                match field.Validate model with
                | Ok (Some v) -> 
                    if v >= limit then
                        v |> Some |> Ok
                    else                 
                        let errorMessage = sprintf "%s must be at greater than %i" field.Name limit
                        Error [ (field.Id, errorMessage) ]
                | Ok None -> Ok None                
                | Error e -> Error e            
            {
                Id = field.Id
                Name = field.Name
                Validate = limitValidation
            }        

        let predicate (pred: 'a -> bool) (buildMessage: string -> string) (field: FieldDefinition<'a option, _>) =
            let validate model =
                match field.Validate model with
                | Ok (Some v) ->
                    if pred v then
                        v |> Some |> Ok
                    else
                        let errorMessage = buildMessage field.Name
                        Error [ (field.Id, errorMessage) ]                
                | Ok None -> Ok None                
                | Error e -> Error e                
            {
                Id = field.Id
                Name = field.Name
                Validate = validate
            }        