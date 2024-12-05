module Kodfodrasz.AoC.Year2024.Day2

open System
open Kodfodrasz.AoC

type parsedinput = int array array

let parseInput (input: string): Result<parsedinput, string> =
  input.Split('\n', StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)
  |> Seq.choose (fun line -> 
    line.Split([|' '; '\t'|],
     StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
      |> Array.map Parse.parseInt
      |> function
      | options when Array.exists ((=) None) options ->           None
      | options ->                     Some (options |> Array.choose id) 
  )
  |> Seq.toArray
  |> Ok 


let answer1 (input : parsedinput) : Result<int,string> =
  let isIncreasing (row : int array) : bool = 
    Seq.pairwise row
    |> Seq.forall (fun (a,b) -> a < b)
  let isDecreasing (row : int array) : bool = 
    Seq.pairwise row
    |> Seq.forall (fun (a,b) -> a > b)
  let isSmallSlope (row : int array) : bool = 
    Seq.pairwise row
    |> Seq.forall (fun (a,b) -> abs(a - b) <= 3)

  let isSafe (row : int array) : bool = 
    (row |> isSmallSlope) && ( row |> isIncreasing || row |> isDecreasing )

  input
  |> Seq.where isSafe
  |> Seq.length
  |> Ok

let answer2  (input : parsedinput)  : Result<int,string>  =
  Error "TODO"

type Solver() =
  inherit SolverBase("Red-Nosed Reports")
  with
    override this.Solve input =
      this.DoSolve parseInput [ answer1; answer2 ] input

