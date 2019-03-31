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

let expect expected actual message =
    if expected <> actual then
        failwithf "Expected %A to be %A\n%s" actual expected message
        
let runTest (name: string) f =
    try
        f()
        Fable.Import.Browser.console.log((sprintf "%%cTest passed: %s" name), "background-color: lightgreen")
    with
    | e ->
        Fable.Import.Browser.console.error((sprintf "Test failed: %s\n%s" name e.Message))

    
let test (fieldId: FieldId) expected =
    let actual = Form.getField2 fieldId state
    if actual = expected then 
        printfn "passed: getField2 %A" expected
    else 
        printfn "failed: Expected: %A Actual: %A" expected actual

let testSet (fieldId: FieldId) expected =
    try 
        let get = Form.getField2 fieldId state
        if get = expected then
            printfn "precondition failed: %A already has expected value" fieldId
        else 
            let state = Form.setField fieldId state expected
            let actual = Form.getField2 fieldId state 
            if actual = expected then 
                printfn "passed: setField %A %A" fieldId expected
            else 
                printfn "failed: Expected: %A Actual: %A" expected actual
    with
    | e ->
        printfn "failed: %A %A" fieldId e
    

module SimpleFormTest =
    let run _ =

        runTest "validator for existing scalar string" <| fun _ ->
            let v = Validator.text "field"
            let model = { Forms.Model.init() with Fields = state.Fields }
            let (Ok result) = Validator.run v model
            expect (Some "value") result "field"
            
        runTest "validator for nonexisting scalar string" <| fun _ ->
            let v = Validator.text "non_existing"
            let model = { Forms.Model.init() with Fields = state.Fields }
            let (Ok result) = Validator.run v model
            expect None result "field"
            
        runTest "validator for two scalar fields" <| fun _ ->
            let v = Validator.from (fun l r -> l,r)
                <*> Validator.text "field"
                <*> Validator.text "field3"
            
            let model = { Forms.Model.init() with Fields = state.Fields }
            let (Ok result) = Validator.run v model
            expect (Some "value", None) result "field"

        runTest "Error messages are formatted by error templates" <| fun _ ->
            let v = Validator.from (fun l r -> l,r)
                <*> (Validator.text "field" |> Validator.required "field")
                <*> (Validator.text "field3" |> Validator.required "field3")
            
            let model = { Forms.Model.init() with Fields = state.Fields }
            let (Error result) = Validator.run v model
            
            let formattedErrors =
                List.map (fun (id,template) -> id, (template id)) result
            
            expect [ "field3", "field3 is required"  ] formattedErrors "field"


        

//TODO: Add/remove to lists via commands
//TODO: Fail when state differs from provided path
let run _ =
    test "field" "value"
    test "field2.nested" "nested_value"
    test "nonextantfield" Forms.Field.defaultValue
    test "leafs.[0]" "leaf0"
    test "leafs.[1]" "leaf1"
    test "leafs.[2]" ""
    test "field2.nested_list.[0]" "nested_list_value"
    
    testSet "field" "value2"
    testSet "field2.nested" "nested_value2"
    testSet "nonextantfield" "new"
    testSet "leafs.[0]" "leaf0_new" //Modify existing list item
    testSet "leafs.[1]" "leaf1_new" //Modify existing list item
    testSet "field2.nested_list.[0]" "nested_list_value_new"
    testSet "nonextantfield.nested" "new"
    
    SimpleFormTest.run ()
    
    
    //testSet "leafs.[2]" "" //Modify non existing list item, I think a pre-condition is adding the item to the list
