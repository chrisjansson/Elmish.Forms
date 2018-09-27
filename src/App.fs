module App

open Elmish
open Elmish.React
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.Core.JsInterop

type Field = string
type FieldId = string

type Model =
    {
        Fields: Map<FieldId, Field>
        Result: string option
    }

type Msg =
    | InputChanged of FieldId * string
    | Submit

let init() : Model = {
    Fields = Map.empty
    Result = None    
}

module FieldId =
    let create (id: string): FieldId = id

module Field =
    let defaultValue: Field = ""

module Form =
    let getField (id: FieldId) (model: Model): Field = 
        Map.tryFind id model.Fields 
        |> Option.defaultValue Field.defaultValue


let firstNameId = FieldId.create "firstName"
let lastNameId = FieldId.create "lastName"

let update (msg:Msg) (model:Model) =
    match msg with
    | InputChanged (id, value) -> 
        { model with Fields = Map.add id value model.Fields }
    | Submit -> 
        { model with Result = Some <| sprintf "%A" (Form.getField firstNameId model, Form.getField lastNameId model) }

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
                    input [ Form.getField firstNameId model |> Value; onChange "firstName" |> OnChange ]
                ]
                div [] [
                    label [] [ unbox "Last name" ]
                    input [ Form.getField lastNameId model |> Value; onChange "lastName" |> OnChange ]
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
