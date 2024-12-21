#!/usr/bin/env dotnet fsi
#r "nuget: Argu"
#r "nuget: FSharp.Data"

open System
open System.IO
open System.Text.RegularExpressions
open Argu
open FSharp.Data

// Constants
let projectYear = 2024
let solutionsFsproj = sprintf "Kodfodrasz.AoC.Year%d/Kodfodrasz.AoC.Year%d.fsproj" projectYear projectYear
let testsFsproj = sprintf "Kodfodrasz.AoC.Year%d.Tests/Kodfodrasz.AoC.Year%d.Tests.fsproj" projectYear projectYear
let templateSolutionFile = sprintf "Kodfodrasz.AoC.Year%d/Templates/DayTemplate.fs" projectYear
let templateTestFile = sprintf "Kodfodrasz.AoC.Year%d.Tests/Templates/DayTestsTemplate.fs" projectYear

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
            failwith "Day not specified and today is not a December day."
    |> function
    | day when  day > 0 && day <= 25 -> day
    | day -> failwithf "The Day specified is not a valid Advent of Code day (1..25): %d" day

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
    let templateContent = File.ReadAllText(templateTestFile)
    let modifiedContent =
        templateContent
          .Replace(sprintf "module private Kodfodrasz.AoC.Year%d.Tests.DayXTests" projectYear, sprintf "module Kodfodrasz.AoC.Year%d.Tests.Day%dTests" projectYear day)
          .Replace(sprintf "open Kodfodrasz.AoC.Year%d.DayX" projectYear, sprintf "open Kodfodrasz.AoC.Year%d.Day%d" projectYear day)
          .Replace("[<Fact(Skip=\"This is a template\")>]", "[<Fact(Skip=\"TODO\")>]")
          .TrimStart()

    printfn "%s" modifiedContent
    modifiedContent

let addFileToFsproj (fsproj: string) (filePath: string) =
    let fsprojContent = File.ReadAllLines(fsproj)

    if not( fsprojContent |> Array.exists (fun s -> s.Contains(filePath))) then
      let lastCompileIndex=
        Array.FindLastIndex(fsprojContent,(fun l -> l.Contains("<Compile Include=")))

      if lastCompileIndex < 0 then
          failwith "No <Compile Include> entries found in the .fsproj file."

      let indent =
          let line = fsprojContent.[lastCompileIndex]
          line.Substring(0, line.IndexOf('<'))

      let newEntry = sprintf "%s<Compile Include=\"%s\" />" indent filePath

      let tryGetDayIndex (s:string) =
          let pattern = @"<Compile Include=""Day(\d+)(Tests)?.fs"""
          let m: Match = Regex.Match(s, pattern)
          if m.Success then
              m.Groups.[1].Value |> int |> Some
          else
              None

      let day = tryGetDayIndex newEntry
      printfn "%A = %A" day newEntry
      let isDayLater s =
          let entryDay = tryGetDayIndex s
          Option.map2 ((>)) day entryDay
          |> Option.defaultValue false

      let insertBeforeIndex = 
          fsprojContent
          |> Array.tryFindIndexBack isDayLater
          |> Option.defaultValue (lastCompileIndex)
          |> (+)1

      let updatedFsproj =
          fsprojContent
          |> Array.toList
          |> List.insertAt (insertBeforeIndex) newEntry
          |> String.concat "\n"

      File.WriteAllText(fsproj, updatedFsproj)
      eprintfn "Added entry for %s in %s" filePath fsproj
    else
      eprintfn "Already present entry for %s in %s" filePath fsproj

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
        
        let dayFilename = sprintf "Day%d.fs" day
        let dayFilepath = sprintf "Kodfodrasz.AoC.Year%d/%s" projectYear dayFilename
        if not <| File.Exists(dayFilepath) then
          let title = fetchQuizTitle day
          printfn "Quiz title for day %d: %s" day title

          let daySolution = templateDaySolution day title
          File.WriteAllText(dayFilepath,daySolution)
          addFileToFsproj solutionsFsproj dayFilename

        let dayTestFilename = sprintf "Day%dTests.fs" day
        let dayTestFilepath = sprintf "Kodfodrasz.AoC.Year%d.Tests/%s" projectYear dayTestFilename
        if not <| File.Exists(dayTestFilepath) then
          printfn "Rendering test"
          let dayTestSolution = templateDayTest day
          File.WriteAllText(dayTestFilepath,dayTestSolution)
          addFileToFsproj testsFsproj dayTestFilename
        else
          printfn "Tests exists"


        // templateDayTest day
        // addFileToFsproj testsFsproj (sprintf "Day%dTests.fs" day)
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
