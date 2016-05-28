// --------------------------------------------------------------------------------------
// FAKE build script
// --------------------------------------------------------------------------------------

#I @"packages/build/FAKE/tools"
#r @"packages/build/FAKE/tools/FakeLib.dll"


let nugetVersion = "0.1.0-alpha1"

open System
open System.IO
open System.Net
open Fake

Target "Build" (fun _ ->
    !! "src/DelayComputationExpression.sln"
    |> MSBuildRelease "" "Rebuild"
    |> ignore
)

Target "CopyToRelease" (fun _ ->
    let files = [ "DelayComputationExpression.dll"; "DelayComputationExpression.xml" ]
    ensureDirectory ("release"@@"lib")
    for f in files do
      CopyFile ("release"@@"lib") ("src"@@"DelayComputationExpression"@@"bin"@@"Release"@@f)
)

Target "Test" (fun _ ->
  ()
)

/// push package (and try again if something fails), FAKE Version doesn't work on mono
/// From https://raw.githubusercontent.com/fsharp/FAKE/master/src/app/FakeLib/NuGet/NugetHelper.fs
let rec private publish parameters =
    let replaceAccessKey key (text : string) =
        if isNullOrEmpty key then text
        else text.Replace(key, "PRIVATEKEY")
    let nuspec = sprintf "%s.%s.nupkg" parameters.Project parameters.Version
    traceStartTask "MyNuGetPublish" nuspec
    let tracing = enableProcessTracing
    enableProcessTracing <- false
    let source =
        if isNullOrEmpty parameters.PublishUrl then ""
        else sprintf "-s %s" parameters.PublishUrl

    let args = sprintf "push \"%s\" %s %s" (parameters.OutputPath @@ nuspec) parameters.AccessKey source
    tracefn "%s %s in WorkingDir: %s Trials left: %d" parameters.ToolPath (replaceAccessKey parameters.AccessKey args)
        (FullName parameters.WorkingDir) parameters.PublishTrials
    try
      try
        let result =
            ExecProcess (fun info ->
                info.FileName <- parameters.ToolPath
                info.WorkingDirectory <- FullName parameters.WorkingDir
                info.Arguments <- args) parameters.TimeOut
        enableProcessTracing <- tracing
        if result <> 0 then failwithf "Error during NuGet push. %s %s" parameters.ToolPath args
        true
      with exn ->
        let existsError = exn.Message.Contains("already exists and cannot be modified")
        if existsError then
          trace exn.Message
          false
        else
          if parameters.PublishTrials > 0 then publish { parameters with PublishTrials = parameters.PublishTrials - 1 }
          else
            (if not (isNull exn.InnerException) then exn.Message + "\r\n" + exn.InnerException.Message
             else exn.Message)
            |> replaceAccessKey parameters.AccessKey
            |> failwith
    finally
      traceEndTask "MyNuGetPublish" nuspec

let packSetup version p =
  { p with
      Authors = ["Matthias Dittrich"]
      Project = "DelayComputationExpression"
      Summary = "F# computation expression library for code which should be executed in a special environment."
      Version = version
      Description = "F# computation expression library for code which should be executed in a special environment."
      Tags = "fsharp library computation expression"
      WorkingDir = "."
      OutputPath = "release"@@"nuget"
      AccessKey = getBuildParamOrDefault "nugetkey" ""
      Publish = false
      Dependencies =
        [ "FSharp.Core" ]
        |> List.map (fun name -> name, (GetPackageVersion "packages" name)) }

Target "PackageNuGet" (fun _ ->
    ensureDirectory ("release"@@"nuget")
    let packSetup = packSetup nugetVersion
    NuGet (fun p -> 
      { (packSetup) p with 
          Publish = false
      }) (Path.Combine("nuget", "DelayComputationExpression.nuspec"))
)

Target "NuGetPush" (fun _ ->
    let packagePushed =
      try
        let packSetup = packSetup nugetVersion
        let parameters = NuGetDefaults() |> (fun p -> 
          { packSetup p with 
              Publish = true })
        // This allows us to specify packages which we do not want to push...
        if hasBuildParam "nugetkey" && parameters.Publish then publish parameters
        else true
      with e -> 
        trace (sprintf "Could not push package '%s': %O" ("DelayComputationExpression") e)
        false

    if not packagePushed then
      failwithf "No package could be pushed!"
)

Target "All" DoNothing
Target "Release" DoNothing

"Build"
  ==> "CopyToRelease"
  ==> "Test"
  ==> "PackageNuGet"
  ==> "All"

"All"
  ==> "NuGetPush"
  ==> "Release"

RunTargetOrDefault "All"


