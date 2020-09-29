#r "paket:
nuget Fake.DotNet.Cli
nuget Fake.IO.FileSystem
nuget Fake.Core.Target //"
#load ".fake/build.fsx/intellisense.fsx"
open Fake.Core
open Fake.DotNet
open Fake.IO
open Fake.IO.Globbing.Operators
open Fake.Core.TargetOperators

Target.initEnvironment ()

Target.create "Clean" (fun _ ->
    !! "Elmish.Forms/**/bin"
    ++ "Elmish.Forms/**/obj"
    ++ "Elmish.Forms.React/**/bin"
    ++ "Elmish.Forms.React/**/obj"
    |> Shell.cleanDirs 
)

Target.create "Build" (fun _ ->
    let packAndPush (project: string) =
        DotNet.pack id (sprintf "./%s" project)
        !! (sprintf "./%s/**/*.nupkg" project)
        |> Seq.iter (fun x -> DotNet.nugetPush (fun options -> options.WithPushParams({ options.PushParams with Source = Some "local" })) x)
    
    packAndPush "./Elmish.Forms"
    packAndPush "./Elmish.Forms.React"
)

Target.create "All" ignore

"Clean"
  ==> "Build"
  ==> "All"

Target.runOrDefault "All"
