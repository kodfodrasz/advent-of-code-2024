module Kodfodrasz.AoC.Year2024.Day11

open System
open System.Collections.Generic
open Kodfodrasz.AoC
open Microsoft.Extensions.Caching.Memory

type parsedinput = int64 list

let parseInput (input: string): Result<parsedinput, string> =
  input.Split([| '\n'; ' '; '\t' |], StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)
  |> Seq.map int64
  |> Seq.toList
  |> Ok

let pluto (n:int64) =
  let str = sprintf "%i" n
  match n with
  | 0L -> [ 1L ]
  | n when str.Length % 2 = 0 -> 
    let halflen= str.Length / 2
    let left = str.Substring(0,halflen) |> int64
    let right = str.Substring(halflen)|> int64
    [ left; right ]
  | _ -> [ n * 2024L ]
  

let answer1 (input : parsedinput) : Result<int,string> =
  // clone for mutation
  let mutable mutato = input
  for i in 1..25 do
    mutato <- List.collect pluto mutato

  List.length mutato
  |> Ok

let answer2 (input : parsedinput) : Result<int,string>  =
  failwith "TODO"

type Solver() =
  inherit SolverBase("Plutonian Pebbles")
  with
    override this.Solve input =
      this.DoSolve parseInput [ answer1; answer2 ] input

