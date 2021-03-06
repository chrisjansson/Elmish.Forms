module Elmish.Forms.Gui

open Elmish.Forms.React
open Feliz

type InputProps =
    {
        Id: FieldId
    }

type prop with
    static member validationFor(id: FieldId) =
        Interop.mkAttr "data-validation-error-for" id

let input' =
    React.functionComponent (fun (props: InputProps) ->
        let field = useField props.Id

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
                    prop.htmlFor props.Id
                ] 
            ]

            Html.div [
                Html.input [
                    prop.placeholder field.Label
                    prop.id props.Id
                    prop.value field.Value
                    prop.onChange field.OnChange
                    prop.onBlur field.TouchField
                ]
            ]
            
            match field.IsTouched, field.ErrorMessage with
            | true, Some errors ->
                let formattedErrorMessage = System.String.Join(", ", errors)
                
                Html.div [
                    Html.label [
                        prop.validationFor props.Id
                        prop.text formattedErrorMessage
                        prop.style [
                            style.color "red"
                        ]
                    ]
                ]
            | _ -> ()
        ]
    )

let input (id: FieldId) = input' { Id = id }
    
    

