module Path

type PathSegment =
    | Node of string
    | List of int

let parse (path: string) =
    let parseNode (node: string ) =
        if node.StartsWith("[") then
            let indexStr = node.Substring(1, node.Length - 2)
            let index = System.Int32.Parse(indexStr)                
            List index
        else            
            Node node
    let rec inner (path: string) acc = 
        let segments = path.Split([| "." |], 2, System.StringSplitOptions.None)
        match segments with
        | [| |] -> acc
        | [| node |] -> 
            parseNode node :: acc                
        | _ ->
            let result = parseNode segments.[0]
            let acc = result :: acc
            inner segments.[1] acc
    inner path [] |> List.rev    

module Tests =
    let test path expected = 
        let actual = parse path
        if actual <> expected then do
            printfn "expected: %A actual: %A" expected actual
        else do        
            printfn "passed: %s" path

    let runTests _ =
        test "node" [ Node "node" ]
        test "node2" [ Node "node2" ]
        test "node0.node1" [ Node "node0"; Node "node1" ]
        test "node0.node1.node2" [ Node "node0"; Node "node1"; Node "node2" ]
        test "node.[0]" [ Node "node"; List 0 ]
        test "node.[0].[2]" [ Node "node"; List 0; List 2 ]
        test "node.[0].node.[2]" [ Node "node"; List 0; Node "node"; List 2 ]