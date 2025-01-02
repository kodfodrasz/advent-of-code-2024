module Kodfodrasz.AoC.Year2024.Day9

open System
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
  let blocks = List<Block>(blocks :> Block seq)
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

let defrag2 blocks : Block array = 
  let blocks = List<Block>(blocks :> Block seq)
  let cmp = BlockPosComparer()
  let freeMap = 
    blocks
    |> Seq.filter(function
      | Free (pos, size) -> size > 0
      | _ -> false)
    |> Seq.groupBy(size)
    |> Map.ofSeq
    |> Map.map(fun k b -> 
      let freeList = List<Block>(b)
      freeList.Sort(cmp)
      freeList
    )

  let files =
    blocks 
    |> Seq.filter(function
      | File(pos, size, id) -> true
      | _ -> false)
    |>  List<Block>
  files.Sort(cmp)
  files.Reverse()

  let rec getFirstFit minSize maxPos = 
    if minSize > 9 then
      None
    else
      
      let idx = 
        if freeMap.ContainsKey minSize then
          freeMap[minSize].FindIndex(function
          | Free (pos, size) -> pos < maxPos
          | _ -> false)
        else -1
      if idx >= 0 then
        let free = freeMap[minSize][idx]
        freeMap[minSize].RemoveAt(idx)
        Some free
      else
        getFirstFit (minSize + 1) maxPos

  let defraggedFiles = 
    files
    |> Seq.map(fun file -> 
      // find first fitting free space.
      // check at the first fitting size, and try larger ones if there is none remaining
      // only consider values which have lower pos than the file!
      match (getFirstFit (size file) (pos file)) with
      | None -> file
      | Some free -> 
          let moved = File(
            pos free,
            size file,
            fid file
          )
          let remSize = size free - size file
          if remSize > 0 then
            freeMap[remSize].Add(Free(
              pos free + size file,
              remSize
              )
            )
            freeMap[remSize].Sort()
          moved
    )

  Seq.concat [ 
    defraggedFiles;
    (freeMap.Values |> Seq.collect (fun fmv -> fmv :> Block seq))]
  |> Seq.sortBy pos
  |> Seq.toArray

let answer2 (blocks : parsedInput) =
  blocks
  |> defrag2
  |> checksum
  |> Ok

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

