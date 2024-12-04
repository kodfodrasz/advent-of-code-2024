module Kodfodrasz.AoC.Year2024.Day1

open System
open Kodfodrasz.AoC


let parseInput (input: string): Result<'a list, string> =
  input.Split('\n')
  |> Seq.choose (fun line -> 
    line.Split([|' '; '\t'|],
     StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
      |> Array.map Parse.parseInt
      |> function
      | [| Some fst ; Some  snd  |] -> Some (fst, snd)
      | _ -> None
  )
  |> Seq.toList
  |> Ok 

let answer1 (rows: (int*int) list) : Result<int,string> =
  let arr1, arr2  = List.unzip rows
  (List.sort arr1, List.sort arr2)
  ||> Seq.zip
  |> Seq.map (fun (a,  b) -> abs(b-a) )
  |> Seq.sum
  |> Ok

let answer2 (rows: (int*int) list) : Result<int,string>  =
  let arr1, arr2  = List.unzip rows
  let arr2counts = List.groupBy id arr2 |> Map.ofList
  
  let eval map (key:int) =
    Map.tryFind key map
    |> Option.map List.length 
    |> Option.defaultValue 0
    |> ((*) key) 

  arr1
  |> List.sumBy( eval arr2counts )
  |> Ok

type Solver() =
  inherit SolverBase("Historian Hysteria")
  with
    override this.Solve input =
      this.DoSolve parseInput [ answer1; answer2 ] input

