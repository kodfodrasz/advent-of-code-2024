[<Kodfodrasz.AoC.AdventOfCode.IgnoreSolver("This is a template. Delete attribute when using")>]
module Kodfodrasz.AoC.Year2024.DayX

open System
open System.Text.RegularExpressions
open Kodfodrasz.AoC

type parsedInput = int list
let parseInput (input: string): Result<parsedInput,string> = 
  Error "TODO"

let answer1 (data : parsedInput) =
  failwith "TODO"

let answer2 (data : parsedInput) =
  failwith "TODO"

type Solver() =
  inherit SolverBase("<SOLVER_NAME>")
  with
    override this.Solve input =
      input
      |>
      this.DoSolve
        (parseInput)
        [ 
          answer1 >> note "THIS IS A TEMPLATE";
          answer2 >> note "THIS IS A TEMPLATE";
        ]

