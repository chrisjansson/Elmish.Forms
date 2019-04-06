module FormTests

open Forms
open Forms.Model

let initial = Forms.Model.init()
let state: Model.Model<unit> = 
    let nestedFields =
        Map.empty
        |> Map.add "nested" (Leaf "nested_value")
        |> Map.add "nested_list" (List ([ Map.ofList ["key", Leaf "nested_list_value"]]))
    let listOfGroups =
        [
            Map.ofList [
                ("inlistf0", Leaf "0_0")
                ("inlistf1", List [ Map.ofList [  "key", Leaf "second_level_list_item" ]; Map.ofList ["key", Leaf "second_level_list_item2"] ])
            ]
            Map.ofList [
                ("inlistf0", Leaf "0_1")
                ("inlistf1", List [ Map.ofList [  "key", Leaf "second_level_list_item3" ]; Map.ofList ["key", Leaf "second_level_list_item4"] ])
            ]
        ]
    let fields = 
        Map.empty 
        |> Map.add "field" (Leaf "value")
        |> Map.add "field2" (Model.Group nestedFields)
        |> Map.add "groupedlist" (Model.List listOfGroups)
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

        runTest "error messages are formatted by error templates" <| fun _ ->
            let v = Validator.from (fun l r -> l,r)
                <*> (Validator.text "field" |> Validator.required "field")
                <*> (Validator.text "field3" |> Validator.required "field3")
            
            let model = { Forms.Model.init() with Fields = state.Fields }
            let (Error result) = Validator.run v model
            
            let formattedErrors =
                List.map (fun (id,template) -> id, (template id)) result
            
            expect [ "field3", "field3 is required"  ] formattedErrors "field"

        runTest "validators can be nested as subvalidators" <| fun _ ->
            let v = Validator.from (fun v nv -> (v, nv))
                <*> Validator.text "field"
                <*> (Validator.withSub "field2" (Validator.text "nested"))
            
            let model = { Forms.Model.init() with Fields = state.Fields }
            let (Ok result) = Validator.run v model
            expect (Some "value", Some "nested_value") result "field"

        runTest "validators can be used as lists" <| fun _ ->
            let v = Validator.from (fun v nv -> (v, nv))
                <*> Validator.text "field"
                <*> (Validator.withList "groupedlist" (Validator.text "inlistf0"))
            
            let model = { Forms.Model.init() with Fields = state.Fields }
            let (Ok result) = Validator.run v model
            expect (Some "value", [ Some "0_0"; Some "0_1"  ]) result "field"

module ListTests =
    let run _ =
        runTest "get list length"  <| fun _ ->
            let result = Form.getListLength "groupedlist" state
            expect 2 result "List length"
            
        runTest "remove item from list"  <| fun _ ->
            let state = Form.removeListItem "groupedlist" 0 state
            let result = Form.getListLength "groupedlist" state
            expect 1 result "List length"
            let value = Form.getField2 "groupedlist.[0].inlistf0" state
            expect "0_1" value "Remaning list item value"
            
        runTest "remove item from list"  <| fun _ ->
            let state = Form.removeListItem "groupedlist.[1].inlistf1" 1 state
            let result = Form.getListLength "groupedlist.[1].inlistf1" state
            expect 1 result "List length"
            let value = Form.getField2 "groupedlist.[1].inlistf1.[0].key" state
            expect "second_level_list_item3" value "Remaning list item value"
        
        runTest "append item to list"  <| fun _ ->
            let state = Form.appendListItem "groupedlist.[1].inlistf1" state
            let result = Form.getListLength "groupedlist.[1].inlistf1" state
            expect 3 result "List length"
            let state = Form.setField "groupedlist.[1].inlistf1.[2].key" state "kaka" 
            let value = Form.getField2 "groupedlist.[1].inlistf1.[2].key" state
            expect "kaka" value "appended list item value"       

//TODO: Add/remove to lists via commands
//TODO: Fail when state differs from provided path
let run _ =
    test "field" "value"
    test "field2.nested" "nested_value"
    test "nonextantfield" Forms.Field.defaultValue
    test "field2.nested_list.[0].key" "nested_list_value"
    
    testSet "field" "value2"
    testSet "field2.nested" "nested_value2"
    testSet "nonextantfield" "new"
    testSet "field2.nested_list.[0].key" "nested_list_value2"

    testSet "field2.nested_list.[0].v" "nested_list_value_new"
    testSet "nonextantfield.nested" "new"
    
    SimpleFormTest.run ()
    ListTests.run ()
    
    
    //testSet "leafs.[2]" "" //Modify non existing list item, I think a pre-condition is adding the item to the list
