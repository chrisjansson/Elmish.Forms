module Elmish.Forms.Schema

let getId (schema: SchemaField) =
    match schema with
    | SchemaField.Leaf ld -> ld.Id
    | SchemaField.Group _ -> failwith "Group has no id"
    | SchemaField.Type _ -> failwith "Type has no id"
    | SchemaField.Sub sd -> sd.Id
    | SchemaField.List ld -> ld.Id
    
let rec getLabel (schema: SchemaField) =
    match schema with
    | SchemaField.Leaf ld -> ld.Label
    | SchemaField.Group gd -> gd.Label
    | SchemaField.Type td -> td.Label
    | SchemaField.Sub sd -> getLabel sd.SubSchema
    | SchemaField.List ld -> getLabel ld.SubSchema
    
let rec withLabel (label: string) (schema: SchemaField) =
    match schema with
    | SchemaField.Leaf ld ->
        SchemaField.Leaf
            {
                ld with Label = Some label
            }
    | SchemaField.Group gd ->
        SchemaField.Group
            {
                gd with Label = Some label
            }
            
    | SchemaField.Type td ->
        SchemaField.Type
            {
                td with Label = Some label
            }
    | SchemaField.Sub sd ->
        SchemaField.Sub
            {
                sd with SubSchema = withLabel label sd.SubSchema
            }
    | SchemaField.List ld ->
        SchemaField.List
            {
                 ld with SubSchema = withLabel label ld.SubSchema
            }
            
let withIsRequired (isRequired: bool) (schema: SchemaField) =
    match schema with
    | SchemaField.Leaf ld ->
        SchemaField.Leaf
            {
                ld with IsRequired = isRequired
            }
    | SchemaField.Group _ -> schema
    | SchemaField.Type _ -> schema
    | SchemaField.Sub _ -> schema
    | SchemaField.List _ -> schema
   
let rec getIsRequired (schema: SchemaField) =
    match schema with
    | SchemaField.Leaf ld -> ld.IsRequired
    | SchemaField.Group _ -> false
    | SchemaField.Type _ -> false
    | SchemaField.Sub _ -> false
    | SchemaField.List _ -> false
    
let rec getType (schema: SchemaField) =
    match schema with
    | SchemaField.Leaf ld -> ld.Type
    | SchemaField.Group gd -> gd.Type
    | SchemaField.Type td -> td.Type
    | SchemaField.Sub td -> sprintf "Sub with %s" (getType td.SubSchema)
    | SchemaField.List ld -> sprintf "List with %s" (getType ld.SubSchema)
    
let withType (t: string) (schema: SchemaField) =
    match schema with
    | SchemaField.Leaf ld -> SchemaField.Leaf { ld with Type = t }
    | SchemaField.Group gd -> SchemaField.Group { gd with Type = t }
    | SchemaField.Type td -> SchemaField.Type { td with Type = t }
    | SchemaField.Sub _ -> schema
    | SchemaField.List _ -> schema
        
let rec getDefaultForSchema (schema: SchemaField) =
    match schema with
    | SchemaField.Leaf l ->
        let defaultValue =
            l.Default
            |> Option.defaultValue ""
        
        Field.Leaf (FieldState.String (defaultValue, { IsTouched = false }))
    | SchemaField.Type t ->
         t.Fields
         |> Map.toList
         |> List.map (fun (id, field) -> id, getDefaultForSchema field)
         |> Map.ofList
         |> Field.Group
    | SchemaField.Group g ->
         g.Fields
         |> Map.toList
         |> List.map (fun (id, field) -> id, getDefaultForSchema field)
         |> Map.ofList
         |> Field.Group
    | SchemaField.Sub s ->
        getDefaultForSchema s.SubSchema
//            Map.ofList [
//                s.Id, getDefaultForSchema s.SubSchema
//            ]
//            |> Field.Group
    | SchemaField.List l ->
        Map.ofList [
            l.Id, Field.List []
        ]
        |> Field.Group
        
    
let getSchemaFromPath (path: FieldId) (model: Model) =
    let path = Path.parse path
    
    let rec inner (pathParts: Path list) (schema: SchemaField) =
        if pathParts.Length = 0 then
            failwith "Empty path"
        else
            match (schema, pathParts) with
            | SchemaField.Leaf l, [ Path.Node leadId ] ->
                if l.Id = leadId then
                    SchemaField.Leaf l
                else
                    failwith "Invalid leaf id"
            | SchemaField.Group g, (Path.Node head)::[] ->
                Map.find head g.Fields
            | SchemaField.Group g, (Path.Node head)::tail ->
                let schema = Map.find head g.Fields
                inner tail schema
            | SchemaField.Sub { Id = subId; SubSchema = schema }, (Path.Node head)::[] when subId = head ->
                schema
            | SchemaField.Sub { Id = subId; SubSchema = schema }, (Path.Node head)::tail when subId = head ->
                inner tail schema
            | SchemaField.List { Id = subId; SubSchema = schema }, (Path.Node head)::[] when subId = head ->
                schema
            | SchemaField.List { Id = subId; SubSchema = schema }, (Path.List (head, _))::tail when subId = head ->
                inner tail schema
            | SchemaField.Type g, (Path.Node head)::[] ->
                Map.find head g.Fields
            | SchemaField.Type g, (Path.Node head)::tail ->
                let schema = Map.find head g.Fields
                inner tail schema
            | _ -> failwithf "Invalid schema path %A actual: %A" path schema
    inner path model.Schema