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


let isIncreasing (row : _ seq) : bool = 
  Seq.pairwise row
  |> Seq.forall (fun (a,b) -> a < b)
let isDecreasing (row : _ seq) : bool = 
  Seq.pairwise row
  |> Seq.forall (fun (a,b) -> a > b)
let isSmallSlope (row : _ seq) : bool = 
  Seq.pairwise row
  |> Seq.forall (fun (a,b) -> abs(a - b) <= 3)

let isSafe (row : _ seq) : bool = 
  (row |> isSmallSlope) && ( row |> isIncreasing || row |> isDecreasing )


let answer1 (input : parsedinput) : Result<int,string> =
  input
  |> Seq.where isSafe
  |> Seq.length
  |> Ok

let skipIndex (arr: 'a array) idx : 'a seq =
  seq {
    for i in arr.GetLowerBound(0) .. arr.GetUpperBound(0) do 
      if i <> idx then
        yield arr[i]
  }

let variations (row: _ array) = 
  seq {0 .. row.Length}
  |> Seq.map (skipIndex row)
  |> Array.ofSeq

let isSafeRelaxed (row : _ array) : bool = 
  row |> variations
  |> Array.Parallel.exists isSafe
  

let answer2  (input : parsedinput)  : Result<int,string>  =
  input
  |> Seq.where isSafeRelaxed
  |> Seq.length
  |> Ok

type Solver() =
  inherit SolverBase("Red-Nosed Reports")
  with
    override this.Solve input =
      this.DoSolve parseInput [ answer1; answer2 ] input

