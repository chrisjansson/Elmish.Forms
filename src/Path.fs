

module Path

type PathSegment =
    | Node of string

let parse (path: string) =
    let rec inner (path: string) acc = 
        let segments = path.Split([| "." |], 2, System.StringSplitOptions.None)
        match segments with
        | [| |] -> acc
        | [| node |] -> Node node :: acc
        | _ ->
            let acc = Node segments.[0] :: acc
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