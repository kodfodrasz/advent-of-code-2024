module Kodfodrasz.AoC.Year2024.Day9

open System
open System.Text.RegularExpressions
open Kodfodrasz.AoC

type Block = 
  | File of pos : int * size : int * id : int 
  | Free of pos : int * size : int

type parsedInput = Block array
let parseInput (input: string): Result<parsedInput,string> = 
  let numbers = 
    input.Split('\n', StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)
    |> Seq.collect(String.toCharArray)
    |> Seq.map (string >> int)
    |> Seq.toArray

  let blocks = 
    numbers
    |> Array.fold (fun (id, pos, l) n -> 
        match l with
        | [] | Free _ :: _ -> 
          let f = File(pos, n, id)
          (id + 1, pos + n, f :: l)
        | File _ :: _->
          let f = Free(pos, n)
          (id, pos + n, f :: l)
        ) (0, 0, List.empty)
    |> (fun (_, _, l) -> List.rev l)

  blocks
  |> List.toArray
  |> Ok

let checksum blocks = 
  Seq.sumBy(function
      | File(pos, _, id) -> int64(pos) * int64(id)
      | Free _ -> 0
  )

let defrag1 blocks : Block array = 
  let free =
    
  failwith "TODO"

let answer1 (blocks : parsedInput) =
  blocks
  |> defrag1
  |> checksum
  |> Ok

let answer2 (data : parsedInput) =
  failwith "TODO"

type Solver() =
  inherit SolverBase("Disk Fragmenter")
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

