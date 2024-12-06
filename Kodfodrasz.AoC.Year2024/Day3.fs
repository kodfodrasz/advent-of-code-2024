module Kodfodrasz.AoC.Year2024.Day3

open System
open Kodfodrasz.AoC
open System.Text.RegularExpressions
open System.Text

type parsedinput = string

let parseInput (input: string): Result<parsedinput, string> =
  // Need to join the lines to properly handle sections where they overlap multiple lines
  input.Split("\n", StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
  |> String.join ""
  |> Ok 

let answer1 (input : parsedinput) : Result<int64,string> =
  // https://regex101.com/r/mdac62/1
  Regex.Matches(input, @"(?<mul>mul\s*\((?<a>\d+)\s*,\s*(?<b>\d+)\s*\))", RegexOptions.ExplicitCapture)
  |> Seq.choose (fun m ->      
      (m.Groups["a"].Value |> Parse.parseInt64,        m.Groups["b"].Value |> Parse.parseInt64 )
      ||> Option.map2 (fun a b -> (a,b)))
  |> Seq.map (fun (a, b) -> a*b)
  |> Seq.sum
  |> Ok

let answer2  (input : parsedinput)  : Result<int64,string>  =
  //
  let sb = StringBuilder(input)
  // Non-greedy capture (.*?) ensures no overlap in matches
  // https://regex101.com/r/m6LzEp/1
  let matches = Regex.Matches(sb.ToString(), @"(?<xclude>don't\(\).*?do\(\))", RegexOptions.ExplicitCapture)

  // remove matches, from back to start to avoid need to recalculate indices
  matches
  |> Seq.sortByDescending( fun m -> m.Index)
  |> Seq.iter( fun m -> 
    sb.Remove(m.Index, m.Length) |> ignore
  )

  // See if any trailing don't remains
  let matches2 = Regex.Matches(sb.ToString(), @"(?<xclude>don't\(\).*?$)", RegexOptions.ExplicitCapture)
  // same code but actually 0|1 result may arise
  matches2
  |> Seq.sortByDescending( fun m -> m.Index)
  |> Seq.iter( fun m -> 
    sb.Remove(m.Index, m.Length) |> ignore
  )

  sb.ToString()
  |> answer1

type Solver() =
  inherit SolverBase("Mull It Over")
  with
    override this.Solve input =
      this.DoSolve parseInput [ answer1; answer2 ] input

