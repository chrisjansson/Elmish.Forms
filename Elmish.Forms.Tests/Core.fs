module ElmishForms.Tests.CoreProperties


open Elmish.Forms
open Elmish.Forms.Core
open Expecto

[<Tests>]
let tests =
    testList "Core properties" [
        let textValidator = Validators.text "fieldId"
        let secondValidator = Validators.text "fieldId2"
        
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
        
        test "Initializes single validator from environment" {
            let textValidator = Validators.text "fieldId"
            let textValidator = textValidator |> Validators.initFrom (fun _ -> Some "hello")
            
            let model = Form.initWithDefault textValidator None

            let actual = Form.validate textValidator () model.FormFields
            let expected = Ok (Some "hello")

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
        
        test "Validates applicative combination" {
            let combined = Validator.from (fun (s1: string option) (s2: string option) -> (s1, s2))
            let combined = Validator.apply combined textValidator
            let combined = Validator.apply combined secondValidator
            
            let model = Form.init combined

            let actual = Form.validate combined () model.FormFields
            let expected = Ok (None, None)

            Expect.equal actual expected "Validation result"
        }
        
        test "Validates applicative combination first required" {
            let textValidator = textValidator |> Validators.isRequired
            let combined = Validator.from (fun (s1: string) (s2: string option) -> (s1, s2))
            let combined = Validator.apply combined textValidator
            let combined = Validator.apply combined secondValidator
            
            let model = Form.init combined

            let actual = Form.validate combined () model.FormFields
            let expected = Error [("fieldId", ["fieldId is required"])]

            Expect.equal actual expected "Validation result"
        }
        
        test "Validates applicative combination second required" {
            let secondValidator = secondValidator |> Validators.isRequired
            let combined = Validator.from (fun (s1: string option) (s2: string) -> (s1, s2))
            let combined = Validator.apply combined textValidator
            let combined = Validator.apply combined secondValidator
            
            let model = Form.init combined

            let actual = Form.validate combined () model.FormFields
            let expected = Error [("fieldId2", ["fieldId2 is required"])]

            Expect.equal actual expected "Validation result"
        }
        
        test "Validates applicative combination both required" {
            let textValidator = textValidator |> Validators.isRequired
            let secondValidator = secondValidator |> Validators.isRequired
            let combined = Validator.from (fun (s1: string) (s2: string) -> (s1, s2))
            let combined = Validator.apply combined textValidator
            let combined = Validator.apply combined secondValidator
            
            let model = Form.init combined

            let actual = Form.validate combined () model.FormFields
            let expected = Error [("fieldId", ["fieldId is required"]); ("fieldId2", ["fieldId2 is required"])]

            Expect.equal actual expected "Validation result"
        }
        
        test "Validates successful applicative combination both required" {
            let textValidator = textValidator |> Validators.isRequired
            let secondValidator = secondValidator |> Validators.isRequired
            let combined = Validator.from (fun (s1: string) (s2: string) -> (s1, s2))
            let combined = Validator.apply combined textValidator
            let combined = Validator.apply combined secondValidator
            
            let model =
                Form.init combined
                |> Form.setField "fieldId" (FieldState.String "hej")
                |> Form.setField "fieldId2" (FieldState.String "hej")

            let actual = Form.validate combined () model.FormFields
            let expected = Ok(("hej", "hej"))

            Expect.equal actual expected "Validation result"
        }
        
        test "Initializes and validates applicative validators" {
            let textValidator = Validators.text "fieldId"
            let secondValidator = Validators.text "fieldId2"
            let textValidator = textValidator |> Validators.isRequired |> Validators.initFrom (fun (x, _) -> x)
            let secondValidator = secondValidator |> Validators.isRequired |> Validators.initFrom (fun (_, y) -> y)
            let combined = Validator.from (fun (s1: string) (s2: string) -> (s1, s2))
            let combined = Validator.apply combined textValidator
            let combined = Validator.apply combined secondValidator
            
            let model = Form.initWithDefault combined ("hello", "world")

            let actual = Form.validate combined () model.FormFields
            let expected = Ok ("hello", "world")

            Expect.equal actual expected "Validation result"
        }
        
        //TODO: Validate combining two validators with the same Id
        
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
            
//            test "Serializes int option to string" {
//                let actual = validator.Serialize (Some (fun x -> Some x)) (Some (Some 4711))
//                
//                let expected = Field.Leaf <| FieldState.String "4711"
//                
//                Expect.equal actual expected "Serializes int"
//            }
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