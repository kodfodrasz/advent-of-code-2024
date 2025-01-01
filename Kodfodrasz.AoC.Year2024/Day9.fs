module Kodfodrasz.AoC.Year2024.Day9

open System
open System.Text.RegularExpressions
open Kodfodrasz.AoC
open System.Collections.Generic

type Block = 
  | File of pos : int * size : int * id : int 
  | Free of pos : int * size : int

let pos = function
| File(pos, _, _) -> pos
| Free(pos, _) -> pos

let size = function
| File(_, size, _) -> size
| Free(_, size) -> size

let fid = function
| File(_, _, _id) -> _id
| Free(_, _) -> failwith "Not a file"

type BlockPosComparer() =
    interface IComparer<Block> with
        member _.Compare(x, y) =
            compare (pos x) (pos y)

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
  ) blocks

let defrag1 blocks : Block array = 
  let blocks2 = List<Block>()
  blocks2.AddRange(blocks)

  let cmp = BlockPosComparer()
  let mutable fragmented = true
  while fragmented do
    let firstFreeIdx = blocks2.FindIndex(function
      | Free (pos, size) -> size > 0
      | _ -> false)
    let lastFileIdx = blocks2.FindLastIndex(function
      | File(pos, size, id) -> true
      | _ -> false)
    
    fragmented <- firstFreeIdx >= 0 && lastFileIdx >= 0

    if fragmented then
      let free = blocks2[firstFreeIdx]
      let file = blocks2[lastFileIdx]

      if size free = size file then
        blocks2[firstFreeIdx] <- File(
          pos free, size file,
          fid file)
        blocks2.RemoveAt(lastFileIdx)
      elif size free > size file then
        blocks2[lastFileIdx] <- File(
          pos free,
          size file,
          fid file)
        blocks2[firstFreeIdx] <- Free(
          pos free + size file,
          size free - size free)
      else // file is bigger
        blocks2[firstFreeIdx] <- File(
          pos free,
          size free,
          fid file)
        blocks2[lastFileIdx] <- File(
          pos file,
          size file - size free,
          fid file)
      blocks2.Sort(cmp)
    
  blocks2.ToArray()

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

