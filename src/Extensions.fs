module Extensions

type ResultBuilder() =
    member this.Bind(x, f) =
        match x with
        | Ok x -> f(x) 
        | Error e -> Error e
    member this.Delay(f) = f()
    member this.Return(x) = Ok x

module Result =
    let apply fr ar =
        match (fr, ar) with
        | (Ok f, Ok a) -> f a |> Ok
        | (Error e, Ok _) -> Error e
        | (Ok _, Error e) -> Error e
        | (Error e1, Error e2) -> List.concat [ e1; e2 ] |> Error

    let (<*>) f a = apply f a

    let ret a = Ok a    

    let lift2 f a b = ret f <*> a <*> b
    let lift3 f a b c = ret f <*> a <*> b <*> c
    let lift4 f a b c d = ret f <*> a <*> b <*> c <*> d
