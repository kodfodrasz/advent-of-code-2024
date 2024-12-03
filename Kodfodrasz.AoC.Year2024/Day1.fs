module Kodfodrasz.AoC.Year2024.Day1

open System
open Kodfodrasz.AoC


let parseInput (input: string): Result<string list, string> =
  input.Split('\n')
  |> Seq.map String.trim
  |> Seq.skipWhile String.isNullOrEmpty
  // no transformation
  |> Seq.where String.notNullOrWhiteSpace
  |> Seq.toList
  |> Ok 

let answer1 (strings: string list) : Result<int,string> =
  Error "TODO"


let answer2  (strings: string list)  : Result<int,string>  =
  Error "TODO"

type Solver() =
  inherit SolverBase("Trebuchet?!")
  with
    override this.Solve input =
      this.DoSolve parseInput [ answer1; answer2 ] input

