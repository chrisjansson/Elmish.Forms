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
        
        test "Has combined schema" {
            let combined = Validator.from (fun (s1: string option) (s2: string option) -> (s1, s2))
            let combined = Validator.apply combined textValidator
            let combined = Validator.apply combined secondValidator

            let expected =
                SchemaField.Type
                    {
                        Type = "Custom validator form FSharpFunc`2"
                        Label = None
                        Fields =
                            [ "fieldId", textValidator.Schema; "fieldId2", secondValidator.Schema ]
                            |> Map.ofList
                    }
            
            Expect.equal combined.Schema expected "Validator schema"
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
                
            test "Initializes form with integer" {
                    let validator = Validators.text "intId" |> Validators.asInt |> Validators.initFrom (fun (i: int) -> Some i)
                    let model = Form.initWithDefault validator 123

                    let actual = Form.validate validator () model.FormFields
                    let expected = Ok (Some 123)

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
            
            for (name, input) in [ "preceding", "  abc"; "succeeding", "abc   " ] do
                test ("Trims whitespace " + name) {
                    let model = Form.init textValidator
                    let model = Form.setField "fieldId" (FieldState.String input) model

                    let actual = Form.validate textValidator () model.FormFields
                    let expected = Ok (Some (input.Trim()))

                    Expect.equal actual expected "Validation result"       
                }
        ]
        
        testList "Sub validators" [
            let nestedValidator = Validator.from (fun (a: string) (b: string) -> (a, b))
            let nestedValidator = Validator.apply nestedValidator (Validators.text "1" |> Validators.isRequired)
            let nestedValidator = Validator.apply nestedValidator (Validators.text "2" |> Validators.isRequired)
            
            let parentValidator = Validator.from (fun (a: string * string) (b: string * string) -> (a, b))
            let parentValidator = Validator.apply parentValidator (Validator.withSub "1" nestedValidator)
            let parentValidator = Validator.apply parentValidator (Validator.withSub "2" nestedValidator)
            
            test "Has correct schema for sub validator" {
                let expected =
                    SchemaField.Type
                        {
                            Type = "Custom validator form FSharpFunc`2"
                            Label = None
                            Fields =
                                [
                                    "1", SchemaField.Sub { Id = "1"; SubSchema = nestedValidator.Schema }
                                    "2", SchemaField.Sub { Id = "2"; SubSchema = nestedValidator.Schema }
                                ]
                                |> Map.ofList
                        }
                Expect.equal parentValidator.Schema expected "Sub validator schema"
            }
            
            test "Initializes form" {
                let model = Form.init parentValidator
                
                
                let expectedModel: Model =
                    let subValidatorDefaults = Map.ofSeq [ "1", Field.Leaf (FieldState.String ""); "2", Field.Leaf (FieldState.String "") ]
                    {
                        FormFields = Map.ofSeq [ "1", Field.Group subValidatorDefaults; "2", Field.Group subValidatorDefaults]
                    }
                    
                Expect.equal model expectedModel "Default initialized model"
            }
            
            test "Initializes with hydrate" {
                let nestedValidator = Validator.from (fun (a: string) (b: string) -> (a, b))
                let nestedValidator = Validator.apply nestedValidator (Validators.text "1" |> Validators.isRequired |> Validators.initFrom (fun (a, _) -> a))
                let nestedValidator: Validator<string * string, _, string * string> = Validator.apply nestedValidator (Validators.text "2" |> Validators.isRequired |> Validators.initFrom (fun (_, b) -> b))
                
                let parentValidator = Validator.from (fun (a: string * string) (b: string * string) -> (a, b))
                let parentValidator = Validator.apply parentValidator (Validator.withSub "1" nestedValidator |> Validators.mapInit (fun (a, _) -> a))
                let parentValidator = Validator.apply parentValidator (Validator.withSub "2" nestedValidator |> Validators.mapInit (fun (_, b) -> b))
                
                let model = Form.initWithDefault parentValidator (("1", "2"), ("3", "4"))
                
                let expectedModel: Model =
                    let subValidator1Defaults = Map.ofSeq [ "1", Field.Leaf (FieldState.String "1"); "2", Field.Leaf (FieldState.String "2") ]
                    let subValidator2Defaults = Map.ofSeq [ "1", Field.Leaf (FieldState.String "3"); "2", Field.Leaf (FieldState.String "4") ]
                    {
                        FormFields = Map.ofSeq [ "1", Field.Group subValidator1Defaults; "2", Field.Group subValidator2Defaults]
                    }
                    
                Expect.equal model expectedModel "Initialized model"
            }
            
            test "Validates correctly" {
                let initialized = Form.init parentValidator
                
                let model =
                    initialized
                    |> Form.setField "1.1" (FieldState.String "11")
                    |> Form.setField "1.2" (FieldState.String "12")
                    |> Form.setField "2.1" (FieldState.String "21")
                    |> Form.setField "2.2" (FieldState.String "22")
                    
                let result = Form.validate parentValidator () model.FormFields
                
                Expect.equal result (Ok (("11", "12"), ("21", "22"))) "Validated result"
            }
            
            test "Maps error message correctly" {
                let initialized = Form.init parentValidator
                
                let model =
                    initialized
                    |> Form.setField "1.1" (FieldState.String "11")
                    |> Form.setField "1.2" (FieldState.String "12")
                    |> Form.setField "2.1" (FieldState.String "21")
                    
                let result = Form.validate parentValidator () model.FormFields
                
                Expect.equal result (Error [("2.2", ["2 is required"])]) "Mapped error message"
            }
        ]
    
        testList "List of text validator" [
            let textValidator = Validators.text "id" |> Validators.isRequired
            
            test "Has correct schema" {
                let validator = Validator.withList "texts" textValidator
                
                let expected =
                    SchemaField.List { Id = "texts"; SubSchema = textValidator.Schema }
                    
                Expect.equal validator.Schema expected "Simple list schema"
            }
            
            test "Default initializes list" {
                let validator = Validator.withList "texts" textValidator
                
                let expected =
                    [
                        "texts", Field.List [ ]
                    ] |> Map.ofList
                
                let model = Form.init validator
                                    
                Expect.equal model.FormFields expected "Default initialized list"
            }
            
            test "Initializes list from data" {
                let validator = Validator.withList "texts" (textValidator |> Validators.initFrom (fun x -> x)) |> Validators.initFrom (fun x -> x)
                
                let expected =
                    [
                        "texts", Field.List [
                            Map.ofList [ "id", Field.Leaf (FieldState.String "hello") ]
                            Map.ofList [ "id", Field.Leaf (FieldState.String "world") ]
                        ]
                    ] |> Map.ofList
                
                let model = Form.initWithDefault validator [ "hello"; "world" ]
                                    
                Expect.equal model.FormFields expected "Default initialized list"
            }
            
            test "Validates list" {
                let validator = Validator.withList "texts" (textValidator |> Validators.initFrom (fun x -> x)) |> Validators.initFrom (fun x -> x)
                
                let model = Form.initWithDefault validator [ "hello"; "world" ]
                                    
                let result = Form.validate validator () model.FormFields
                
                let expected = Ok ([ "hello"; "world" ])
                
                Expect.equal result expected "Validated result"
            }
            
            test "Validates invalid list" {
                let validator = Validator.withList "texts" (textValidator |> Validators.initFrom (fun x -> x)) |> Validators.initFrom (fun x -> x)
                
                let model = Form.initWithDefault validator [ "hello"; "" ]
                                    
                let result = Form.validate validator () model.FormFields
                
                let expected = Error [("texts.[1].texts", ["texts is required"])]
                
                Expect.equal result expected "Validated result"
            }
        ]
    ]