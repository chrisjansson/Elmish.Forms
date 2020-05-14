module ElmishForms.Tests.CoreProperties


open Elmish.Forms
open Elmish.Forms.Core
open Expecto

[<Tests>]
let tests =
    testList "Core properties" [
        let textValidator = Validators.text "fieldId"
        
        test "Can add label to schema meta data" {
            let validator = Validators.withLabel "a label" textValidator
            
            let expected = SchemaField.Leaf { Id = "fieldId"; Label = Some "a label"; Type = "string"; IsRequired = false }
            let actual = validator.Schema
            
            Expect.equal actual expected "Validator schema with label"
        }

        test "Initializes model from single validator" {
            let model = Form.init textValidator
            
            let expectedModel: Model =
                {
                    FormFields = Map.ofSeq [ "fieldId", Field.Leaf (FieldState.String "") ]
                }
                
            Expect.equal model expectedModel "Initialized model"
        }
        
        test "Validates single validator" {
            let model = Form.init textValidator

            let actual = Form.validate textValidator () model.FormFields
            let expected = Ok None

            Expect.equal actual expected "Validation result"          
        }
        
        test "Validates required validator and formats label" {
            let requiredValidator =
                textValidator
                |> Validators.isRequired 
                |> Validators.withLabel "Required field"
            
            let model = Form.init requiredValidator

            let actual = Form.validate requiredValidator () model.FormFields
            let expected = Error [ "fieldId", [ "Required field is required" ] ]

            Expect.equal actual expected "Validation result"      
        }
        
        //TODO: predicate validator
        
        testList "Int validator" [
            let validator = textValidator |> Validators.asInt

            test "Has schema" {
                
                let expected = SchemaField.Leaf { Id = "fieldId"; Label = None; Type = "int"; IsRequired = false }
                let actual = validator.Schema
                
                Expect.equal actual expected "Validator schema"
            }
            
            for (input, expected) in [ "1", 1; "132", 132; "-32", -32 ] do
                test ("Validates valid integer " + input) {
                    
                    let model = Form.init validator
                    let model = Form.setField "fieldId" (FieldState.String input) model

                    let actual = Form.validate validator () model.FormFields
                    let expected = Ok (Some expected)

                    Expect.equal actual expected "Validation result"   
                }
                
            for input in [ "abc"; "1.2"; "-123a" ] do
                test ("Rejects invalid integers " + input) {
                    
                    let model = Form.init validator
                    let model = Form.setField "fieldId" (FieldState.String input) model

                    let actual = Form.validate validator () model.FormFields
                    let expected = Error [ "fieldId", [ "fieldId should be a valid number" ] ]

                    Expect.equal actual expected "Validation result"   
                }
        ]
        
        testList "Text validator" [
            let validator = Validators.text "fieldId"

            test "Has schema" {
                
                let expected = SchemaField.Leaf { Id = "fieldId"; Label = None; Type = "string"; IsRequired = false }
                let actual = validator.Schema
                
                Expect.equal actual expected "Validator schema"
            }
            
            for (name,case) in [ "Empty string", ""; "White space", "  " ] do
                test ("Validates empty as none" + name) {
                    let model = Form.init textValidator
                    let model = Form.setField "fieldId" (FieldState.String case) model

                    let actual = Form.validate textValidator () model.FormFields
                    let expected = Ok None

                    Expect.equal actual expected "Validation result"          
                }
        
            test "Validates any other string as some" {
                let model = Form.init textValidator
                let model = Form.setField "fieldId" (FieldState.String "hello world") model

                let actual = Form.validate textValidator () model.FormFields
                let expected = Ok (Some "hello world")

                Expect.equal actual expected "Validation result"       
            }
            
            for (name, input) in [ "preceeding", "  abc"; "succeeding", "abc   " ] do
                test ("Trims whitespace " + name) {
                    let model = Form.init textValidator
                    let model = Form.setField "fieldId" (FieldState.String input) model

                    let actual = Form.validate textValidator () model.FormFields
                    let expected = Ok (Some (input.Trim()))

                    Expect.equal actual expected "Validation result"       
                }
        ]
    ]