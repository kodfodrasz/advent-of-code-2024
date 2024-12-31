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
      | File(pos, size, id) -> 
          let mutable sum = 0L
          for x in 0 .. size - 1 do
            sum <- sum + int64(pos + x) * int64(id)
          sum 
      | Free _ -> 0
  ) blocks

let defrag1 blocks : Block array = 
  let blocks = System.Collections.Generic.List<Block>(blocks :> Block seq)

  let cmp = BlockPosComparer()
  let mutable fragmented = true
  while fragmented do
    let firstFreeIdx = blocks.FindIndex(function
      | Free (pos, size) -> size > 0
      | _ -> false)
    let lastFileIdx = blocks.FindLastIndex(function
      | File(pos, size, id) -> true
      | _ -> false)
    
    fragmented <- firstFreeIdx >= 0 && lastFileIdx >= 0

    if fragmented then
      let free = blocks[firstFreeIdx]
      let file = blocks[lastFileIdx]

      if pos free > pos file then
        blocks.RemoveAt(firstFreeIdx)
      elif size free = size file then
        blocks[firstFreeIdx] <- File(
          pos free,
          size file,
          fid file)
        blocks.RemoveAt(lastFileIdx)
      elif size free > size file then
        blocks[lastFileIdx] <- File(
          pos free,
          size file,
          fid file)
        blocks[firstFreeIdx] <- Free(
          pos free + size file,
          size free - size file)
      else // file is bigger
        blocks[firstFreeIdx] <- File(
          pos free,
          size free,
          fid file)
        blocks[lastFileIdx] <- File(
          pos file,
          size file - size free,
          fid file)
      blocks.Sort(cmp)
    
  blocks.ToArray()

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

