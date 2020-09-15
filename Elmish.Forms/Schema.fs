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
    
let rec getType (schema: SchemaField) =
    match schema with
    | SchemaField.Leaf ld -> ld.Type
    | SchemaField.Group gd -> gd.Type
    | SchemaField.Type td -> td.Type
    | SchemaField.Sub td -> sprintf "Sub with %s" (getType td.SubSchema)
    
let withType (t: string) (schema: SchemaField) =
    match schema with
    | SchemaField.Leaf ld -> SchemaField.Leaf { ld with Type = t }
    | SchemaField.Group gd -> SchemaField.Group { gd with Type = t }
    | SchemaField.Type td -> SchemaField.Type { td with Type = t }
    | SchemaField.Sub _ -> schema