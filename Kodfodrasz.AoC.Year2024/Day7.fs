module Kodfodrasz.AoC.Year2024.Day7

open System
open System.Text.RegularExpressions
open Kodfodrasz.AoC

type CalibrationEquation = {
  TestValue : int64
  Terms: int64 list
}

type parsedInput = CalibrationEquation list
let parseInput (input: string): Result<parsedInput,string> = 
  let matchesEquatons= Regex.Matches(
    input,
    @"^\s*(?<testvalue>\d+):(?:\s+(?<n>\d+))+\s*$",
    RegexOptions.Multiline)

  let matchedEquations = 
    let parseRule (m:Match) = 
      let tv = m.Groups["testvalue"].Value |> int64
      let terms = 
        m.Groups["n"].Captures 
        |> Seq.map (fun (c:Capture) -> c.Value |> int64)
        |> Seq.toList

      {
        TestValue = tv
        Terms = terms
      }

    matchesEquatons
    |> Seq.map parseRule
    |> Seq.toList
  
  Ok matchedEquations

let check (eqn : CalibrationEquation) = 
  let rec check goal acc terms = 
    match terms with
    | [] -> goal = acc
    | head :: tail ->
      if acc > goal then false
      else 
        if (check goal (acc + head) tail) then
          true
        else
          check goal (acc * head) tail
  match eqn.Terms with
  | [] -> false
  | h :: t -> check eqn.TestValue h t

let answer1 (data : parsedInput) =
  data
  |> List.toArray
  |> Array.Parallel.filter check
  |> Array.sumBy(fun eqn -> eqn.TestValue)
  |> Ok 

let answer2 (data : parsedInput) =
  failwith "TODO"

type Solver() =
  inherit SolverBase("Bridge Repair")
  with
    override this.Solve input =
      input
      |>
      this.DoSolve
        (parseInput)
        [ 
          answer1;
          answer2;
        ]

