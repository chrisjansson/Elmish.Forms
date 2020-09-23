module Elmish.Forms.Gui

open Elmish.Forms.React
open Feliz

let input (id: FieldId) =
    let field = useField id

    Html.div [
        let label = field.Label
        let label =
            if field.IsRequired then
                sprintf "%s *" label
            else
                label
        
        Html.div [
            Html.label [
                prop.text label
                prop.htmlFor id
            ] 
        ]

        Html.div [
            Html.input [
                prop.placeholder field.Label
                prop.id id
                prop.value field.Value
                prop.onChange field.OnChange
            ]
        ]
    ]

