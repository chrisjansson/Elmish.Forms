module App.Samples.ChooseSample

open Elmish.Forms
open Elmish.Forms.Validator.Operators
open Feliz

let source: string = Fable.Core.JsInterop.importDefault (Utils.importLiteral + __SOURCE_FILE__)

type Shape =
    | Circle of int
    | Square of int
    | Rectangle of int * int

type ShapeEnum =
    | Circle = 0
    | Square = 1
    | Rectangle = 2

let validator: Validator<ShapeEnum * Shape, unit, _> =
    
    let circle =
        Validator.from (fun radius -> Circle radius)
        <*> (Validator.Standard.text "radius" |> Validator.Standard.asInt |> Validator.isRequired |> Validator.withLabel "Radius")
    
    let square =
        Validator.from (fun side -> Square side)
        <*> (Validator.Standard.text "side" |> Validator.Standard.asInt |> Validator.isRequired |> Validator.withLabel "Side")
    
    let rectangle =
        Validator.from (fun width height -> Rectangle (width, height))
        <*> (Validator.Standard.text "width" |> Validator.Standard.asInt |> Validator.isRequired |> Validator.withLabel "Width")
        <*> (Validator.Standard.text "height" |> Validator.Standard.asInt |> Validator.isRequired |> Validator.withLabel "Height")
    
    Validator.choose
        ShapeEnum.Circle 
        (fun e -> string e)
        [
            ShapeEnum.Circle, circle
            ShapeEnum.Square, square
            ShapeEnum.Rectangle, rectangle
        ]

let render () =
    React.fragment [
        let option (value: ShapeEnum) (text: string) =
            Html.option [
                prop.text text
                prop.value (string value)
            ]
        
        let selectField = React.useField "discriminator"
        
        Html.select [
            prop.value selectField.Value
            prop.onChange selectField.OnChange
            prop.children [
                option ShapeEnum.Circle "Circle"
                option ShapeEnum.Rectangle "Rectangle"
                option ShapeEnum.Square "Square"
            ]
        ]
        
        match selectField.Value with
        | "0" -> Gui.input "0.radius"
        | "1" -> Gui.input "1.side"
        | "2" ->
            Gui.input "2.width"
            Gui.input "2.height"
        | _ -> ()
    ]