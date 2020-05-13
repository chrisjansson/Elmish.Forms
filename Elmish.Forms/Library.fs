namespace Elmish.Forms

//Validation error formatter
// 'ValidationErrorContext -> string list

//A validation function takes a validation environment, the value and returns a new value
// 'value_in -> 'env -> Result<'value_out, ValidationErrorFormatter>

//A validator
//Validator<'Result, 'Env, 'InitializeFrom>
// {
//    Validate: FormData -> 'Env -> Result
//    Schema: A description of the forms structure, includes metadata such as labels, type, requiredness, static default values
//
//
//
//
// }

module Say =
    let hello name =
        printfn "Hello %s" name
