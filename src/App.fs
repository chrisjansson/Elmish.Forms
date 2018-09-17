module App

open Elmish
open Elmish.React
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.Core.JsInterop

type Model =
    {
        FirstName: string
        LastName: string
        Result: string option
    }

type Msg =
    | InputChanged of string * string
    | Submit

let init() : Model = {
    FirstName = ""
    LastName = ""
    Result = None    
}

let update (msg:Msg) (model:Model) =
    match msg with
    | InputChanged ("firstName", value) -> { model with FirstName = value }
    | InputChanged ("lastName", value) -> { model with LastName = value }
    | Submit -> 
        { model with Result = Some <| sprintf "%A" (model.FirstName, model.LastName) }
    | _ -> model

let view (model:Model) dispatch =
    let onChange field (event: Fable.Import.React.FormEvent) =
        let value = string event.currentTarget?value
        InputChanged (field, value) |> dispatch

    let onSubmit (event: Fable.Import.React.FormEvent) = 
        event.preventDefault()
        dispatch Submit    
    
    div []
        [
            form [ OnSubmit onSubmit ] [
                div [] [
                    label [] [ unbox "First name" ]
                    input [ Value model.FirstName; onChange "firstName" |> OnChange ]
                ]
                div [] [
                    label [] [ unbox "Last name" ]
                    input [ Value model.LastName; onChange "lastName" |> OnChange ]
                ]
                button [ Type "submit" ] [ unbox "Submit" ]
            ]

            pre [] [
                (match model.Result with
                | Some r -> unbox r
                | None -> unbox "")
            ]
        ]

Program.mkSimple init update view
    |> Program.withReact "elmish-app"
    |> Program.withConsoleTrace
    |> Program.run
