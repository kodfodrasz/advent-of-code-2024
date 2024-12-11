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
  | 0L -> [| 1L |]
  | n when str.Length % 2 = 0 -> 
    let halflen= str.Length / 2
    let left = str.Substring(0,halflen) |> int64
    let right = str.Substring(halflen)|> int64
    [| left; right |]
  | _ -> [| n * 2024L |]
  

let answer1 (input : parsedinput) : Result<int64,string> =
  // clone for mutation
  let mutable mutato = input |> List.toArray
  for i in 1..25 do
    mutato <- Array.collect pluto mutato

  Array.length mutato
  |> int64
  |> Ok


let prepareLUT depth =
  let empty = Array.empty<int64>
  // depth 0 : the numbers themselves. small waste, easier indexing 
  let lut = Array2D.create 10 (depth+1) empty 
  
  for num in 0..9 do
    let mutable nash = [| int64 num |]
    lut[num, 0] <- nash
    for depth in 1..depth do
      nash <- Array.collect pluto nash // Not that terrible film actually
      lut[num, depth] <- nash
    
  lut

let rec dive (lut : int64 array array2d) depth num =
  if depth = 0 then
    1L
  else 
    let str = sprintf "%i" num
    match str.Length with
    | 1 -> 
        let max_step = (Array2D.length2 lut) - 1
        if depth <= max_step then
          // we can directly answer
          Array.length lut[int num, depth] |> int64
        else
          let leap = lut[int num, max_step]
          let remaining_depth = depth - max_step
          // (numbers, count) list
          let groups = 
            Array.groupBy id leap 
            |> Array.map (fun (num, arr) -> num, Array.length arr)
            |> Map.ofArray

          groups
          |> Map.map (fun num count -> (int64 count) * (dive lut remaining_depth num))
          |> Map.values 
          |> Seq.sum
    | n when str.Length % 2 = 0 -> 
        let halflen= str.Length / 2
        let left = str.Substring(0,halflen) |> int64
        let right = str.Substring(halflen)|> int64
        let leap = [| left; right |]
        let groups = 
          Array.groupBy id leap 
          |> Array.map (fun (num, arr) -> num, Array.length arr)
          |> Map.ofArray
        groups
        |> Map.map (fun num count -> (int64 count) * (dive lut (depth - 1) num))
        |> Map.values 
        |> Seq.sum
    | _ -> 
        dive lut (depth - 1) (num * 2024L)

let answer2_raw lutDepth calcDepth (input : parsedinput) : Result<int64,string> =
  let lut = prepareLUT lutDepth

  input 
  |> Array.ofList
  |> Array.Parallel.map (dive lut calcDepth)
  |> Seq.sum
  |> Ok

let answer2 =
  // bad: 862423061625454 TOO HIGH
  // Good:241651071960597
  answer2_raw 25 75


type Solver() =
  inherit SolverBase("Plutonian Pebbles")
  with
    override this.Solve input =
      this.DoSolve parseInput [ answer1; answer2 ] input

