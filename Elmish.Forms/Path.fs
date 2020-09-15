[<RequireQualifiedAccess>]
module Elmish.Forms.Path

open System.Text.RegularExpressions

let private tryParseListAccess node =
    let r = Regex("(.+)\[([0-9+])\]")
    let matches = r.Matches(node)
    if matches.Count = 0 then
        None
    else
        Some (matches.[0].Groups.[1].Value, int (matches.[0].Groups.[2].Value))

let parse (path: FieldId) =
    let pathParts = path.Split([|'.'|])
    
    [
        for part in pathParts do
            match tryParseListAccess part with
            | Some (id, index) -> Path.List (id, index)
            | None -> Path.Node part
    ]