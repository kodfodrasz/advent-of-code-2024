#r "nuget: Argu"
#r "nuget: FSharp.Data"

open System
open System.IO
open Argu
open FSharp.Data

// Constants
let projectYear = 2024
let solutionsFsproj = "Kodfodrasz.AoC.Year2024/Kodfodrasz.AoC.Year2024.fsproj"
let testsFsproj = "tests.fsproj"
let templateSolutionFile = "Kodfodrasz.AoC.Year2024/Templates/DayTemplate.fs"
let templateTestFile = "DayTestTemplate.fs"

// Argument parser
type CLIArgs =
    | Day of int
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Day _ -> "Specifies the day (integer). If not provided, assumes today's date if in December."

let getDayFromArguments (parsedArguments:ParseResults<CLIArgs>) =
    match parsedArguments.TryGetResult CLIArgs.Day with
    | Some day -> day
    | None ->
        let today = DateTime.Now
        if today.Month = 12 && today.Year = projectYear then
            today.Day
        else
            failwith "Day not specified and today is not a valid December day."

let templateDaySolution (day: int) (solverName: string) =
    let templateContent = File.ReadAllText(templateSolutionFile)
    let modifiedContent =
        templateContent
          .Replace("[<Kodfodrasz.AoC.AdventOfCode.IgnoreSolver(\"This is a template. Delete attribute when using\")>]", "")
          .Replace(sprintf "module Kodfodrasz.AoC.Year%d.DayX" projectYear, sprintf "module Kodfodrasz.AoC.Year%d.Day%d" projectYear day)
          .Replace("<SOLVER_NAME>", solverName)
          .Replace(" >> note \"THIS IS A TEMPLATE\"", "")
          .TrimStart()

    printfn "%s" modifiedContent
    modifiedContent

let templateDayTest (day: int) =
    // TODO: Implement functionality to generate a test file for the given day
    printfn "Generating test file for day %d" day

let addFileToFsproj (fsproj: string) (filePath: string) =
    let fsprojContent = File.ReadAllLines(fsproj)

// TODO: insert the entry sorted...
// only insert if the entry does not exist
    let lastCompileIndex=
      Array.FindLastIndex(fsprojContent,(fun l -> l.Contains("<Compile Include=")))

    if lastCompileIndex < 0 then
        failwith "No <Compile Include> entries found in the .fsproj file."

    let indent =
        let line = fsprojContent.[lastCompileIndex]
        line.Substring(0, line.IndexOf('<'))

    let newEntry = sprintf "%s<Compile Include=\"%s\" />" indent filePath

    let updatedFsproj =
        fsprojContent
        |> Array.toList
        |> List.insertAt (lastCompileIndex + 1) newEntry
        |> String.concat "\n"

    File.WriteAllText(fsproj, updatedFsproj)
    eprintfn "Added entry for %s in %s" filePath fsproj

let fetchQuizTitle (day: int) : string =
    let url = sprintf "https://adventofcode.com/%d/day/%d" projectYear day
    let html = HtmlDocument.Load(url)
    let titleNode = html.CssSelect("body main article.day-desc h2").Head
    match titleNode.InnerText().Split(':') |> Array.tryLast with
    | Some title -> title.Trim(' ', '-')
    | None -> failwith "Could not extract the title."

// Main logic
//[<EntryPoint>]
let main (argv) =
    try
        let scriptname = Environment.GetCommandLineArgs()[1]
        let parser = ArgumentParser.Create<CLIArgs>(programName=scriptname)
        let parsedArgs = parser.Parse (argv)

        let day = getDayFromArguments parsedArgs
        let title = fetchQuizTitle day
        
        printfn "Quiz title for day %d: %s" day title
        
        let daySolution = templateDaySolution day title
        let dayFilename = sprintf "Day%d.fs" day
        let dayFilepath = sprintf "Kodfodrasz.AoC.Year%d/%s" projectYear dayFilename
        if not <| File.Exists(dayFilepath) then
          File.WriteAllText(dayFilepath,daySolution)
          addFileToFsproj solutionsFsproj (sprintf "Day%d.fs" day)


        templateDayTest day
        addFileToFsproj testsFsproj (sprintf "Day%dTests.fs" day)
        printfn "Day %d setup completed successfully." day
        0
    with
    | :? ArguParseException as e -> eprintfn "%s" e.Message; 127
    | ex ->
        eprintfn "Error: %s" ex.Message
        1

//main Command
let argv = 
  Environment.GetCommandLineArgs() 
  |> Array.skip 2 // 1 for fsi.dll, another for the script name
main argv
