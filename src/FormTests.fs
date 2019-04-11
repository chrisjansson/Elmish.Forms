module FormTests

open Forms
open Forms.Model
open Forms.Validator

let inList =
    Validator.from (fun v -> v)
    <*> (Validator.text "key")

let nested =
    Validator.from (fun f l -> (f, l))
    <*> (Validator.text "nested")
    <*> (Validator.withList "nested_list" inList)

let initV =
    Validator.from (fun (f: string) _-> f)
    <*> (Validator.text "field" |> Validator.required)
    <*> (Validator.withSub "field2" nested)


let initial = Forms.Model.init initV
let state: Model.Model<_> = initial
//    let nestedFields =
//        Map.empty
//        |> Map.add "nested" (Leaf "nested_value")
//        |> Map.add "nested_list" (List ([ Map.ofList ["key", Leaf "nested_list_value"] ], Leaf ""))
//    let listOfGroups =
//        [
//            Map.ofList [
//                ("inlistf0", Leaf "0_0")
//                ("inlistf1", Leaf "010123")
//            ]
//            Map.ofList [
//                ("inlistf0", Leaf "0_1")
//                ("inlistf1", Leaf "asd")
//            ]
//        ]
//    let fields = 
//        Map.empty 
//        |> Map.add "field" (Leaf "value")
//        |> Map.add "field2" (Model.Group nestedFields)
//        |> Map.add "groupedlist" (Model.List (listOfGroups, Leaf ""))
//    { 
//        initial with Fields = fields
//    }

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

    
let test (fieldId: FieldId) expected state =
    let actual = Form.getFieldValue fieldId state
    if actual = expected then 
        printfn "passed: getField2 %A" expected
    else 
        printfn "failed: Expected: %A Actual: %A" expected actual

let testSet (fieldId: FieldId) expected state =
    let get = Form.getFieldValue fieldId state
    if get = expected then
        printfn "precondition failed: %A already has expected value" fieldId
    else 
        let state = Form.setFieldValue fieldId state expected
        let actual = Form.getFieldValue fieldId state 
        if actual = expected then 
            printfn "passed: setField %A %A" fieldId expected
        else 
            printfn "failed: Expected: %A Actual: %A" expected actual
    

module SimpleFormTest =
    let run _ =

        runTest "validator for existing scalar string" <| fun _ ->
            let v = Validator.text "field"
            let model = { Forms.Model.init v with Fields = state.Fields }
            let (Ok result) = Validator.run v model
            expect (Some "value") result "field"
            
        runTest "validator for nonexisting scalar string" <| fun _ ->
            let v = Validator.text "non_existing"
            let model = { Forms.Model.init v with Fields = state.Fields }
            let (Ok result) = Validator.run v model
            expect None result "field"
            
        runTest "validator for two scalar fields" <| fun _ ->
            let v = Validator.from (fun l r -> l,r)
                <*> Validator.text "field"
                <*> Validator.text "field3"
            
            let model = { Forms.Model.init v with Fields = state.Fields }
            let (Ok result) = Validator.run v model
            expect (Some "value", None) result "field"

        runTest "error messages are formatted by error templates" <| fun _ ->
            let v = Validator.from (fun l r -> l,r)
                <*> (Validator.text "field" |> Validator.required)
                <*> (Validator.text "field3" |> Validator.required)
            
            let model = { Forms.Model.init v with Fields = state.Fields }
            let (Error result) = Validator.run v model
            
            let formattedErrors = result
            
            expect [ "field3", "field3 is required"  ] formattedErrors "field"

        runTest "validators can be nested as subvalidators" <| fun _ ->
            let v = Validator.from (fun v nv -> (v, nv))
                <*> Validator.text "field"
                <*> (Validator.withSub "field2" (Validator.text "nested"))
            
            let model = { Forms.Model.init v with Fields = state.Fields }
            let (Ok result) = Validator.run v model
            expect (Some "value", Some "nested_value") result "field"

        runTest "validators can be used as lists" <| fun _ ->
            let v = Validator.from (fun v nv -> (v, nv))
                <*> Validator.text "field"
                <*> (Validator.withList "groupedlist" (Validator.text "inlistf0"))
            
            let model = { Forms.Model.init v with Fields = state.Fields }
            let (Ok result) = Validator.run v model
            expect (Some "value", [ Some "0_0"; Some "0_1"  ]) result "field"

module ListTests =
    let run _ =
        runTest "get list length"  <| fun _ ->
            let result = Form.getListLength "field2.nested_list" state
            expect 0 result "List length"

        runTest "append item to list"  <| fun _ ->
            let state = Form.appendListItem "field2.nested_list" state
            let result = Form.getListLength "field2.nested_list" state
            expect 1 result "List length"
            let state = Form.setFieldValue "field2.nested_list.[0].key" state "kaka" 
            let value = Form.getFieldValue "field2.nested_list.[0].key" state
            expect "kaka" value "appended list item value"       

        let state = Form.appendListItem "field2.nested_list" state

        runTest "remove item from list"  <| fun _ ->
            let state = Form.removeListItem "field2.nested_list" 0 state
            let result = Form.getListLength "groupedlist" state
            expect 0 result "List length"
            
//        runTest "remove item from list"  <| fun _ ->
//            let state = Form.removeListItem "groupedlist.[1].inlistf1" 1 state
//            let result = Form.getListLength "groupedlist.[1].inlistf1" state
//            expect 1 result "List length"
//            let value = Form.getField "groupedlist.[1].inlistf1.[0].key" state
//            expect "second_level_list_item3" value "Remaning list item value"
//        

module InitTests =
    let run _ =
        runTest "init text field" <| fun _ ->
            let (Forms.Validator.Validator {Default = d}) = Forms.Validator.text "test"
            
            let expected = Model.Group <| Map.ofList [ "test", Model.Leaf "" ]
            
            expect expected d "default value for single leaf"
            
        runTest "init two text fields" <| fun _ ->
            let v = Validator.from (fun l r -> l,r)
                <*> Validator.text "field"
                <*> Validator.text "field3"
                
            let (Validator.Validator {Default = defaultValue}) = v
            
            
            let expected = Model.Group <| Map.ofList [
                "field", Model.Leaf ""
                "field3", Model.Leaf ""
            ]
            
            expect expected defaultValue "default value for two leafs"

        runTest "init subgroup" <| fun _ ->
            let v = Validator.from (fun v nv -> (v, nv))
                <*> Validator.text "field"
                <*> (Validator.withSub "field2" (Validator.text "nested"))
                
            let (Validator.Validator {Default = defaultValue}) = v

            let expected = Model.Group <| Map.ofList [
                "field", Model.Leaf ""
                "field2", Model.Group <| Map.ofList [
                    "nested", Model.Leaf ""
                ]
            ]
            
            expect expected defaultValue "default value for groups"
        
        runTest "init list" <| fun _ ->
            let v = Validator.from (fun v nv -> (v, nv))
                <*> Validator.text "field"
                <*> (Validator.withList "groupedlist" (Validator.text "inlistf0"))
                        
            let (Validator.Validator {Default = defaultValue}) = v
                        
            let expected = Model.Group <| Map.ofList [
                "field", Model.Leaf ""
                "groupedlist", Model.List <| ([], Map.ofList [ "inlistf0", Model.Leaf "" ])
            ]
            
            expect expected defaultValue "field"

//TODO: Fail when state differs from provided path
let run _ =
    test "field" "" state
    try
        test "nonextantfield" "" state
    with _ ->
        printfn "passed: fetching nonextantfield"
        ()
        
    test "field2.nested" "" state
    
    let state = Form.appendListItem "field2.nested_list" state
    test "field2.nested_list.[0].key" "" state

    testSet "field" "value2" state
    testSet "field2.nested" "nested_value2" state
    try 
        testSet "nonextantfield" "new" state
    with _ ->
        printfn "passed: setting non extant field errors"
        ()
    testSet "field2.nested_list.[0].key" "nested_list_value2" state
    
    SimpleFormTest.run ()
    ListTests.run ()
    InitTests.run ()
