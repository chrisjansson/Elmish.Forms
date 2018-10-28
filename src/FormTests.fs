module FormTests

open Forms
open Forms.Model


let initial = Forms.Model.init()
let state: Model.Model<unit> = 
    let nestedFields =
        Map.empty
        |> Map.add "nested" (Leaf "nested_value")
        |> Map.add "nested_list" (List [(Leaf "nested_list_value")])
    let listOfLeafs =
        [
            Leaf "leaf0"
            Leaf "leaf1"
        ]    
    let fields = 
        Map.empty 
        |> Map.add "field" (Leaf "value")
        |> Map.add "field2" (Model.Group nestedFields)
        |> Map.add "leafs" (Model.List listOfLeafs)
    { 
        initial with Fields = fields
    }


let test (fieldId: FieldId) expected =
    let actual = Form.getField2 fieldId state
    if actual = expected then 
        printfn "passed: getField2 %A" expected
    else 
        printfn "failed: Expected: %A Actual: %A" expected actual

let run _ =
    test "field" "value"
    test "field2.nested" "nested_value"
    test "nonextantfield" Forms.Field.defaultValue
    test "leafs.[0]" "leaf0"
    test "leafs.[1]" "leaf1"
    test "leafs.[2]" ""
    test "field2.nested_list.[0]" "nested_list_value"
    ()