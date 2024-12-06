module Kodfodrasz.AoC.Year2024.Day3

open System
open Kodfodrasz.AoC
open System.Text.RegularExpressions

type parsedinput = string

let parseInput (input: string): Result<parsedinput, string> =
  input
  |> String.trim
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
  failwith "TODO"

type Solver() =
  inherit SolverBase("Mull It Over")
  with
    override this.Solve input =
      this.DoSolve parseInput [ answer1; answer2 ] input

