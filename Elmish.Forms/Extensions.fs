[<AutoOpen>]
module Extensions

[<RequireQualifiedAccess>]
module Result =
    let traverse (v: List<Result<_, _>>) =
        let reducer left right =
            match left, right with
            | Ok l, Ok r -> Ok (l::r)
            | Ok _, Error e -> Error e
            | Error e, Ok _ -> Error e
            | Error l, Error r -> Error (l@r)
            
        List.foldBack reducer v (Ok [])
