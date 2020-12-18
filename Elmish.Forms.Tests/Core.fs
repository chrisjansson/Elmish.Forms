module ElmishForms.Tests.CoreProperties


open Elmish.Forms
open Expecto


[<Tests>]
let tests =
    testList "Core properties" [
        let textValidator = Validator.Standard.text "fieldId"
        let secondValidator = Validator.Standard.text "fieldId2"
        
        test "Can add label to schema meta data" {
            let validator = Validator.withLabel "a label" textValidator
            
            let expected = SchemaField.Leaf { Id = "fieldId"; Label = Some "a label"; Type = "string"; IsRequired = false; Default = None }
            let actual = validator.Schema
            
            Expect.equal actual expected "Validator schema with label"
        }

        test "Initializes model from single validator" {
            let model = Form.init textValidator
            
            let expectedModel: Model =
                {
                    FormFields = Map.ofSeq [ "fieldId", Field.Leaf (FieldState.String ("", { IsTouched = false })) ]
                    Schema = model.Schema
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
            let textValidator = Validator.Standard.text "fieldId"
            let textValidator = textValidator |> Validator.initFrom (fun _ -> Some "hello")
            
            let model = Form.initWithDefault textValidator None

            let actual = Form.validate textValidator () model.FormFields
            let expected = Ok (Some "hello")

            Expect.equal actual expected "Validation result"          
        }
        
        test "Validates required validator and formats label" {
            let requiredValidator =
                textValidator
                |> Validator.isRequired 
                |> Validator.withLabel "Required field"
            
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
            let textValidator = textValidator |> Validator.isRequired
            let combined = Validator.from (fun (s1: string) (s2: string option) -> (s1, s2))
            let combined = Validator.apply combined textValidator
            let combined = Validator.apply combined secondValidator
            
            let model = Form.init combined

            let actual = Form.validate combined () model.FormFields
            let expected = Error [("fieldId", ["fieldId is required"])]

            Expect.equal actual expected "Validation result"
        }
        
        test "Validates applicative combination second required" {
            let secondValidator = secondValidator |> Validator.isRequired
            let combined = Validator.from (fun (s1: string option) (s2: string) -> (s1, s2))
            let combined = Validator.apply combined textValidator
            let combined = Validator.apply combined secondValidator
            
            let model = Form.init combined

            let actual = Form.validate combined () model.FormFields
            let expected = Error [("fieldId2", ["fieldId2 is required"])]

            Expect.equal actual expected "Validation result"
        }
        
        test "Validates applicative combination both required" {
            let textValidator = textValidator |> Validator.isRequired
            let secondValidator = secondValidator |> Validator.isRequired
            let combined = Validator.from (fun (s1: string) (s2: string) -> (s1, s2))
            let combined = Validator.apply combined textValidator
            let combined = Validator.apply combined secondValidator
            
            let model = Form.init combined

            let actual = Form.validate combined () model.FormFields
            let expected = Error [("fieldId", ["fieldId is required"]); ("fieldId2", ["fieldId2 is required"])]

            Expect.equal actual expected "Validation result"
        }
        
        test "Validates successful applicative combination both required" {
            let textValidator = textValidator |> Validator.isRequired
            let secondValidator = secondValidator |> Validator.isRequired
            let combined = Validator.from (fun (s1: string) (s2: string) -> (s1, s2))
            let combined = Validator.apply combined textValidator
            let combined = Validator.apply combined secondValidator
            
            let model =
                Form.init combined
                |> Form.setField "fieldId" "hej"
                |> Form.setField "fieldId2" "hej"

            let actual = Form.validate combined () model.FormFields
            let expected = Ok(("hej", "hej"))

            Expect.equal actual expected "Validation result"
        }
        
        
        test "Applicative bind fail" {
            let textValidator = textValidator |> Validator.isRequired |> Validator.withLabel "hej"
            let secondValidator = secondValidator |> Validator.isRequired
            let combined = Validator.from (fun (s1: string) (s2: string) -> (s1, s2))
            let combined = Validator.apply combined textValidator
            let combined = Validator.apply combined secondValidator
            let combined = Validator.bind  (fun v c -> Error [ "", [ "an error on the group" ] ]) id combined
            
            let model =
                Form.init combined
                |> Form.setField "fieldId" "hej"
                |> Form.setField "fieldId2" "hej"

            let actual = Form.validate combined () model.FormFields
            let expected = Error [ "", [ "an error on the group" ] ]

            Expect.equal actual expected "Validation result"
        }
        
        test "Initializes and validates applicative validators" {
            let textValidator = Validator.Standard.text "fieldId"
            let secondValidator = Validator.Standard.text "fieldId2"
            let textValidator = textValidator |> Validator.isRequired |> Validator.initFrom (fun (x, _) -> x)
            let secondValidator = secondValidator |> Validator.isRequired |> Validator.initFrom (fun (_, y) -> y)
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
            let validator = textValidator |> Validator.Standard.asInt

            test "Has schema" {
                
                let expected = SchemaField.Leaf { Id = "fieldId"; Label = None; Type = "int"; IsRequired = false; Default = None }
                let actual = validator.Schema
                
                Expect.equal actual expected "Validator schema"
            }
            
            for (input, expected) in [ "1", 1; "132", 132; "-32", -32 ] do
                test ("Validates valid integer " + input) {
                    
                    let model = Form.init validator
                    let model = Form.setField "fieldId" input model

                    let actual = Form.validate validator () model.FormFields
                    let expected = Ok (Some expected)

                    Expect.equal actual expected "Validation result"   
                }
                
                
            for input in [ "abc"; "1.2"; "-123a" ] do
                test ("Rejects invalid integers " + input) {
                    let model = Form.init validator
                    let model = Form.setField "fieldId" input model

                    let actual = Form.validate validator () model.FormFields
                    let expected = Error [ "fieldId", [ "fieldId should be a valid number" ] ]

                    Expect.equal actual expected "Validation result"   
                }
                
            test "Initializes form with integer" {
                    let validator = Validator.Standard.text "intId" |> Validator.Standard.asInt |> Validator.initFrom (fun (i: int) -> Some i)
                    let model = Form.initWithDefault validator 123

                    let actual = Form.validate validator () model.FormFields
                    let expected = Ok (Some 123)

                    Expect.equal actual expected "Validation result"   
                }
        ]
        
        testList "Text validator" [
            let validator = Validator.Standard.text "fieldId"

            test "Has schema" {
                
                let expected = SchemaField.Leaf { Id = "fieldId"; Label = None; Type = "string"; IsRequired = false; Default = None }
                let actual = validator.Schema
                
                Expect.equal actual expected "Validator schema"
            }
            
            for (name,case) in [ "Empty string", ""; "White space", "  " ] do
                test ("Validates empty as none" + name) {
                    let model = Form.init textValidator
                    let model = Form.setField "fieldId" case model

                    let actual = Form.validate textValidator () model.FormFields
                    let expected = Ok None

                    Expect.equal actual expected "Validation result"          
                }
        
            test "Validates any other string as some" {
                let model = Form.init textValidator
                let model = Form.setField "fieldId" "hello world" model

                let actual = Form.validate textValidator () model.FormFields
                let expected = Ok (Some "hello world")

                Expect.equal actual expected "Validation result"       
            }
            
            for (name, input) in [ "preceding", "  abc"; "succeeding", "abc   " ] do
                test ("Trims whitespace " + name) {
                    let model = Form.init textValidator
                    let model = Form.setField "fieldId" input model

                    let actual = Form.validate textValidator () model.FormFields
                    let expected = Ok (Some (input.Trim()))

                    Expect.equal actual expected "Validation result"       
                }
                
            test "Validates successfully with bind" {
                let validator =
                    validator
                    |> Validator.bind (fun s _ -> Ok s) id
                
                let actual =
                    Form.init validator
                    |> Form.setField "fieldId" "Hej"
                    |> (fun x -> Form.validate validator () x.FormFields)
                
                Expect.equal (Ok (Some "Hej")) actual "result"
            }
            
            test "Fails validation with error from bind" {
                let validator =
                    validator
                    |> Validator.bind (fun s context -> Error [ Schema.getId context.Schema, [ "an error" ] ]) id
                
                let actual =
                    Form.init validator
                    |> Form.setField "fieldId" "Hej"
                    |> (fun x -> Form.validate validator () x.FormFields)
                
                Expect.equal (Error [ "fieldId", [ "an error" ] ]) actual "result"
            }
        ]
        
        testList "Sub validators" [
            let nestedValidator = Validator.fromNamed "nested" (fun (a: string) (b: string) -> (a, b))
            let nestedValidator = Validator.apply nestedValidator (Validator.Standard.text "1" |> Validator.isRequired)
            let nestedValidator = Validator.apply nestedValidator (Validator.Standard.text "2" |> Validator.isRequired)
            
            let parentValidator = Validator.fromNamed "parent" (fun (a: string * string) (b: string * string) -> (a, b))
            let parentValidator = Validator.apply parentValidator (Validator.withSub "1" nestedValidator)
            let parentValidator = Validator.apply parentValidator (Validator.withSub "2" nestedValidator)
            
            test "Has correct schema for sub validator" {
                let expected =
                    SchemaField.Type
                        {
                            Type = "parent"
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
                    let subValidatorDefaults = Map.ofSeq [
                        "1", Field.Leaf (FieldState.String ("", { IsTouched = false }))
                        "2", Field.Leaf (FieldState.String ("", { IsTouched = false }))
                    ]
                    
                    {
                        FormFields = Map.ofSeq [
                            "1", Field.Group subValidatorDefaults
                            "2", Field.Group subValidatorDefaults
                        ]
                        Schema = model.Schema
                    }
                    
                Expect.equal model expectedModel "Default initialized model"
            }
            
            test "Initializes with hydrate" {
                let nestedValidator = Validator.from (fun (a: string) (b: string) -> (a, b))
                let nestedValidator = Validator.apply nestedValidator (Validator.Standard.text "1" |> Validator.isRequired |> Validator.initFrom (fun (a, _) -> a))
                let nestedValidator: Validator<string * string, _, string * string> = Validator.apply nestedValidator (Validator.Standard.text "2" |> Validator.isRequired |> Validator.initFrom (fun (_, b) -> b))
                
                let parentValidator = Validator.from (fun (a: string * string) (b: string * string) -> (a, b))
                let parentValidator = Validator.apply parentValidator (Validator.withSub "1" nestedValidator |> Validator.mapInit (fun (a, _) -> a))
                let parentValidator = Validator.apply parentValidator (Validator.withSub "2" nestedValidator |> Validator.mapInit (fun (_, b) -> b))
                
                let model = Form.initWithDefault parentValidator (("1", "2"), ("3", "4"))
                
                let expectedModel: Model =
                    let subValidator1Defaults = Map.ofSeq [ "1", Field.Leaf (FieldState.String ("1", { IsTouched = false })); "2", Field.Leaf (FieldState.String ("2", { IsTouched = false })) ]
                    let subValidator2Defaults = Map.ofSeq [ "1", Field.Leaf (FieldState.String ("3", { IsTouched = false })); "2", Field.Leaf (FieldState.String ("4", { IsTouched = false })) ]
                    {
                        FormFields = Map.ofSeq [ "1", Field.Group subValidator1Defaults; "2", Field.Group subValidator2Defaults]
                        Schema = model.Schema
                    }
                    
                Expect.equal model expectedModel "Initialized model"
            }
            
            test "Validates correctly" {
                let initialized = Form.init parentValidator
                
                let model =
                    initialized
                    |> Form.setField "1.1" "11"
                    |> Form.setField "1.2" "12"
                    |> Form.setField "2.1" "21"
                    |> Form.setField "2.2" "22"
                    
                let result = Form.validate parentValidator () model.FormFields
                
                Expect.equal result (Ok (("11", "12"), ("21", "22"))) "Validated result"
            }
            
            test "Maps error message correctly" {
                let initialized = Form.init parentValidator
                
                let model =
                    initialized
                    |> Form.setField "1.1" "11"
                    |> Form.setField "1.2" "12"
                    |> Form.setField "2.1" "21"
                    
                let result = Form.validate parentValidator () model.FormFields
                
                Expect.equal result (Error [("2.2", ["2 is required"])]) "Mapped error message"
            }
        ]
    
        testList "List of text validator" [
            let textValidator = Validator.Standard.text "id" |> Validator.isRequired
            
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
                let validator = Validator.withList "texts" (textValidator |> Validator.initFrom (fun x -> x)) |> Validator.initFrom (fun x -> x)
                
                let expected =
                    [
                        "texts", Field.List [
                            Map.ofList [ "id", Field.Leaf (FieldState.String ("hello", { IsTouched = false })) ]
                            Map.ofList [ "id", Field.Leaf (FieldState.String ("world", { IsTouched = false })) ]
                        ]
                    ] |> Map.ofList
                
                let model = Form.initWithDefault validator [ "hello"; "world" ]
                                    
                Expect.equal model.FormFields expected "Default initialized list"
            }
            
            test "Validates list" {
                let validator = Validator.withList "texts" (textValidator |> Validator.initFrom (fun x -> x)) |> Validator.initFrom (fun x -> x)
                
                let model = Form.initWithDefault validator [ "hello"; "world" ]
                                    
                let result = Form.validate validator () model.FormFields
                
                let expected = Ok ([ "hello"; "world" ])
                
                Expect.equal result expected "Validated result"
            }
            
            test "Validates invalid list" {
                let validator = Validator.withList "texts" (textValidator |> Validator.initFrom (fun x -> x)) |> Validator.initFrom (fun x -> x)
                
                let model = Form.initWithDefault validator [ "hello"; "" ]
                                    
                let result = Form.validate validator () model.FormFields
                
                let expected = Error [("texts[1].id", ["id is required"])]
                
                Expect.equal result expected "Validated result"
            }

            test "Add list item" {
                let validator = Validator.withList "texts" (textValidator |> Validator.initFrom (fun x -> x)) |> Validator.initFrom (fun x -> x)
                
                let model =
                    Form.initWithDefault validator [ ]
                    |> Form.addListItem "texts"
                                    
                let result = Form.validate validator () model.FormFields
                
                let expected = Error [("texts[0].id", ["id is required"])]
                
                Expect.equal result expected "Validated result"
            }

            test "Remove list item" {
                let validator = Validator.withList "texts" (textValidator |> Validator.initFrom (fun x -> x)) |> Validator.initFrom (fun x -> x)
                
                let model =
                    Form.initWithDefault validator [ ]
                    |> Form.addListItem "texts"
                    |> Form.removeListItem "texts" 0
                                    
                let result = Form.validate validator () model.FormFields
                
                let expected = Ok []
                
                Expect.equal result expected "Validated result"
            }
                        
            test "Validates, set values" {
                let validator = Validator.withList "texts" (textValidator |> Validator.initFrom (fun x -> x)) |> Validator.initFrom (fun x -> x)
                
                let model =
                    Form.initWithDefault validator [ ""; "" ]
                    |> Form.setField "texts[0].id" "hello"
                    |> Form.setField "texts[1].id" "world"
                                    
                let result = Form.validate validator () model.FormFields
                
                let expected = Ok ([ "hello"; "world" ])
                
                Expect.equal result expected "Validated result"
            }
        ]
        
        testList "List of complex validator" [
            let textValidator = Validator.Standard.text "id" |> Validator.isRequired |> Validator.initFrom (fun (a, _) -> a)
            let textValidator2 = Validator.Standard.text "id2" |> Validator.isRequired |> Validator.initFrom (fun (_, b) -> b)
            
            let complexValidator =
                Validator.from (fun a b -> (a, b))
                |> (fun v -> Validator.apply v textValidator)
                |> (fun v -> Validator.apply v textValidator2)
            
            test "Has correct schema" {
                let validator = Validator.withList "complex" complexValidator
                
                let expected =
                    SchemaField.List { Id = "complex"; SubSchema = complexValidator.Schema }
                    
                Expect.equal validator.Schema expected "Complex list validator schema"
            }
            
            test "Default initializes list" {
                let validator = Validator.withList "complex" complexValidator
                
                let expected =
                    [
                        "complex", Field.List [ ]
                    ] |> Map.ofList
                
                let model = Form.init validator
                                    
                Expect.equal model.FormFields expected "Default initialized list"
            }

            test "Initializes list from data" {
                let validator = Validator.withList "complex" complexValidator |> Validator.initFrom (fun x -> x)
                
                let expected =
                    [
                        "complex", Field.List [
                            Map.ofList [ "id", Field.Leaf (FieldState.String ("hello", { IsTouched = false })); "id2", Field.Leaf (FieldState.String ("world", { IsTouched = false })) ]
                        ]
                    ] |> Map.ofList
                
                let model = Form.initWithDefault validator [ "hello", "world" ]
                                    
                Expect.equal model.FormFields expected "Default initialized list"
            }

            test "Validates list" {
                let validator = Validator.withList "complex" complexValidator |> Validator.initFrom (fun x -> x)
                
                let model = Form.initWithDefault validator [ "hello", "world" ]
                                    
                let result = Form.validate validator () model.FormFields
                
                let expected = Ok ([ "hello", "world" ])
                
                Expect.equal result expected "Validated result"
            }
            
            test "Validates invalid list" {
                let validator = Validator.withList "complex" complexValidator |> Validator.initFrom (fun x -> x)
                
                let model = Form.initWithDefault validator [ "hello", "" ]
                                    
                let result = Form.validate validator () model.FormFields
                
                let expected = Error [("complex[0].id2", ["id2 is required"])]
                
                Expect.equal result expected "Validated result"
            }
            
            test "Add list item" {
                let validator = Validator.withList "complex" complexValidator |> Validator.initFrom (fun x -> x)
                
                let model =
                    Form.initWithDefault validator [ ]
                    |> Form.addListItem "complex"
                                    
                let result = Form.validate validator () model.FormFields
                
                let expected = Error [
                    ("complex[0].id", ["id is required"])
                    ("complex[0].id2", ["id2 is required"])
                ]
                
                Expect.equal result expected "Validated result"
            }

            test "Remove list item" {
                let validator = Validator.withList "complex" complexValidator |> Validator.initFrom (fun x -> x)
                
                let model =
                    Form.initWithDefault validator [ ]
                    |> Form.addListItem "complex"
                    |> Form.removeListItem "complex" 0
                                    
                let result = Form.validate validator () model.FormFields
                
                let expected = Ok []
                
                Expect.equal result expected "Validated result"
            }
        ]
        
        testList "Choose" [
            let option1 = Validator.Standard.text "opt1"
            let option2 = Validator.Standard.text "opt2"
            
            let validator = Validator.choose "1" id [
                "1", option1
                "2", option2
            ]            
            
            test "Default initializes" {
                let model = Form.init validator
                
                let expected =
                    Map.ofList [
                        "discriminator", Field.Leaf (FieldState.String ("1", { IsTouched = false }))
                        "1", Field.Group (Map.ofList [ "opt1", Field.Leaf (FieldState.String ("",  { IsTouched = false })) ])
                        "2", Field.Group (Map.ofList [ "opt2", Field.Leaf (FieldState.String ("", { IsTouched = false })) ])
                    ]
                    
                Expect.equal model.FormFields expected "Default value of choose"
            }
            
            test "Initializes select" {
                let option1 = Validator.Standard.text "opt1" |> Validator.initFrom id
                let option2 = Validator.Standard.text "opt2" |> Validator.initFrom id
                
                let validator =
                    Validator.choose "1" id [
                        "1", option1
                        "2", option2
                    ] |> Validator.initFrom id
                
                let model = Form.initWithDefault validator ("2", Some "henlo")
                
                let expected =
                    Map.ofList [
                        "discriminator", Field.Leaf (FieldState.String ("2", { IsTouched = false }))
                        "1", Field.Group (Map.ofList [ "opt1", Field.Leaf (FieldState.String ("", { IsTouched = false })) ])
                        "2", Field.Group (Map.ofList [ "opt2", Field.Leaf (FieldState.String ("henlo", { IsTouched = false })) ])
                    ]
                    
                Expect.equal model.FormFields expected "Default value of choose"
            }
        
            test "Validates second option of choose" {
                let option1 = Validator.Standard.text "opt1" |> Validator.initFrom id
                let option2 = Validator.Standard.text "opt2" |> Validator.initFrom id
                
                let validator =
                    Validator.choose "1" id [
                        "1", option1
                        "2", option2
                    ] |> Validator.initFrom id
                
                
                let actual =
                    Form.initWithDefault validator ("2", Some "henlo")
                    |> (fun x -> Form.validate validator () x.FormFields) 

                let expected = Ok ("2", Some "henlo")
                                    
                Expect.equal actual expected "Validated value"
            }
            
            test "Selects option 2, sets value then validates" {
                let option1 = Validator.Standard.text "opt1" |> Validator.initFrom id
                let option2 = Validator.Standard.text "opt2" |> Validator.initFrom id
                
                let validator =
                    Validator.choose "1" id [
                        "1", option1
                        "2", option2
                    ] |> Validator.initFrom id
                
                
                let actual =
                    Form.init validator
                    |> Form.setField "discriminator" "2"
                    |> Form.setField "1.opt1" "option 1"
                    |> Form.setField "2.opt2" "option 2"
                    |> (fun x -> Form.validate validator () x.FormFields) 

                let expected = Ok ("2", Some "option 2")
                                    
                Expect.equal actual expected "Validated value"
            }
            
            test "Selecting required option without entering the field is invalid" {
                let option1 = Validator.Standard.text "opt1" |> Validator.isRequired |> Validator.initFrom id
                let option2 = Validator.Standard.text "opt2" |> Validator.isRequired |> Validator.initFrom id
                
                let validator =
                    Validator.choose "1" id [
                        "1", option1
                        "2", option2
                    ] |> Validator.initFrom id
                
                let actual =
                    Form.init validator
                    |> Form.setField "discriminator" "2"
                    |> Form.setField "1.opt1" "option 1"
                    |> (fun x -> Form.validate validator () x.FormFields) 

                let expected = Error [ "2.opt2", [ "opt2 is required" ] ]
                                    
                Expect.equal actual expected "Validated value"
            }
        ]
    
    
        testList "Complex validator with list" [
            let textValidator = Validator.Standard.text "id" |> Validator.isRequired |> Validator.initFrom fst
            let textValidator2 = Validator.Standard.text "id2" |> Validator.isRequired |> Validator.initFrom snd
            
            let complexValidator =
                Validator.from (fun a b -> (a, b))
                |> (fun v -> Validator.apply v textValidator)
                |> (fun v -> Validator.apply v textValidator2)
                
            let listValidator =
                Validator.from (fun a b -> (a, b))
                |> (fun v -> Validator.apply v textValidator)
                |> (fun v -> Validator.apply v (Validator.withList "list" complexValidator))
            
            test "Has correct schema" {
                let validator = listValidator
                
                let expected =
                    SchemaField.Type
                        {
                            Type = "Custom validator form FSharpFunc`2"
                            Label = None
                            Fields = Map.ofList [
                                "id", SchemaField.Leaf { Id = "id"; Label = None; Type = "string"; IsRequired = true; Default = None }
                                "list", SchemaField.List { Id = "list"; SubSchema = complexValidator.Schema }
                            ]
                        }
                    
                Expect.equal validator.Schema expected "Complex list validator schema"
            }
            
            test "Default initializes list" {
                let validator = listValidator
                
                let expected =
                    [
                        "id", Field.Leaf (FieldState.String ("", { IsTouched = false }))
                        "list", Field.List [ ]
                    ] |> Map.ofList
                
                let model = Form.init validator
                                    
                Expect.equal model.FormFields expected "Default initialized list"
            }

            test "Initializes list from data" {
                
                                
                let listValidator: Validator<string * ((string * string) list), _, string * ((string * string) list)> =
                    Validator.from (fun (a: string) (b: (string * string) list) -> (a, b))
                    |> (fun v -> Validator.apply v (Validator.Standard.text "id" |> Validator.isRequired |> Validator.initFrom fst))
                    |> (fun v -> Validator.apply v ((Validator.withList "list" (complexValidator |> Validator.initFrom id) |> Validator.initFrom snd) |> Validator.initFrom snd))

                let validator = listValidator
                
                let expected =
                    [
                        "id", Field.Leaf (FieldState.String ("hello world", { IsTouched = false }))
                        "list", Field.List [
                            Map.ofList [ "id", Field.Leaf (FieldState.String ("hello", { IsTouched = false }))
                                         "id2", Field.Leaf (FieldState.String ("world", { IsTouched = false })) ]
                        ]
                    ] |> Map.ofList
                
                let model = Form.initWithDefault validator ("hello world", [ "hello", "world" ])
                                    
                Expect.equal model.FormFields expected "Default initialized list"
            }

            test "Validates list" {
                let listValidator: Validator<string * ((string * string) list), _, string * ((string * string) list)> =
                    Validator.from (fun (a: string) (b: (string * string) list) -> (a, b))
                    |> (fun v -> Validator.apply v (Validator.Standard.text "id" |> Validator.isRequired |> Validator.initFrom fst))
                    |> (fun v -> Validator.apply v ((Validator.withList "list" (complexValidator |> Validator.initFrom id) |> Validator.initFrom snd) |> Validator.initFrom snd))

                let validator = listValidator
                
                let model = Form.initWithDefault validator ("hello world", [ "hello", "world" ])
                                    
                let result = Form.validate validator () model.FormFields
                
                let expected = Ok ("hello world", [ "hello", "world" ])
                
                Expect.equal result expected "Validated result"
            }
       
            test "Validates invalid list" {
                let listValidator: Validator<string * ((string * string) list), _, string * ((string * string) list)> =
                    Validator.from (fun (a: string) (b: (string * string) list) -> (a, b))
                    |> (fun v -> Validator.apply v (Validator.Standard.text "id" |> Validator.isRequired |> Validator.initFrom fst))
                    |> (fun v -> Validator.apply v ((Validator.withList "list" (complexValidator |> Validator.initFrom id) |> Validator.initFrom snd) |> Validator.initFrom snd))
                
                let validator = listValidator
                
                let model = Form.initWithDefault validator ("hello", [ "", "world" ])
                                    
                let result = Form.validate validator () model.FormFields
                
                let expected = Error [("list[0].id", ["id is required"])]
                
                Expect.equal result expected "Validated result"
            }

            test "Add list item 1" {
                let listValidator: Validator<string * ((string * string) list), _, string * ((string * string) list)> =
                    Validator.from (fun (a: string) (b: (string * string) list) -> (a, b))
                    |> (fun v -> Validator.apply v (Validator.Standard.text "id" |> Validator.isRequired |> Validator.initFrom fst))
                    |> (fun v -> Validator.apply v ((Validator.withList "list" (complexValidator |> Validator.initFrom id) |> Validator.initFrom snd) |> Validator.initFrom snd))
                
                let validator = listValidator
                
                let model =
                    Form.initWithDefault validator ("hello", [ ])
                    |> Form.addListItem "list"
                                    
                let result = Form.validate validator () model.FormFields
                
                let expected = Error [
                    ("list[0].id", ["id is required"])
                    ("list[0].id2", ["id2 is required"])
                ]
                
                Expect.equal result expected "Validated result"
            }

            test "Remove list item" {
                let listValidator: Validator<string * ((string * string) list), _, string * ((string * string) list)> =
                    Validator.from (fun (a: string) (b: (string * string) list) -> (a, b))
                    |> (fun v -> Validator.apply v (Validator.Standard.text "id" |> Validator.isRequired |> Validator.initFrom fst))
                    |> (fun v -> Validator.apply v ((Validator.withList "list" (complexValidator |> Validator.initFrom id) |> Validator.initFrom snd) |> Validator.initFrom snd))
                
                let validator = listValidator
                
                let model =
                    Form.initWithDefault validator ("hello", [ ])
                    |> Form.addListItem "list"
                    |> Form.removeListItem "list" 0
                                    
                let result = Form.validate validator () model.FormFields
                
                let expected = Ok ("hello", [])
                
                Expect.equal result expected "Validated result"
            }
        ]
    ]