module Kodfodrasz.AoC.Year2024.Day6

open System
open System.Text.RegularExpressions
open Kodfodrasz.AoC

type parsedInput = char array array // input[row][column]

let parseInput (input: string): Result<parsedInput, string> =
  input.Split('\n', StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)
  |> Seq.map String.toCharArray
  |> Seq.toArray
  |> Ok

type coords = int * int
type direction = North | East | South | West

let rec walk (visited : Map<coords,direction>) (map : char array array) (pos:coords) (dir:direction) : Map<coords,direction> =
  let outOfBounds pos = 
    let r, c = pos
    r < 0 || c < 0 || r >= map.Length || c >= map[0].Length

  let isObstructed aim =
    let r, c = aim
    if outOfBounds aim then false
    else map[r][c] = '#'

  if outOfBounds pos then
    Map.remove pos visited
  else
    let r, c = pos
    let aim = 
      match dir with
      | North -> r - 1, c
      | East -> r, c + 1
      | South -> r + 1, c
      | West -> r, c - 1
    if not(isObstructed aim) then
      walk (Map.add pos dir visited) map aim dir
    else
      let turnedDir = 
        match dir with
        | North -> East
        | East -> South
        | South -> West
        | West -> North
      walk visited map pos turnedDir

let answer1 (data : parsedInput) =
  let map  = data // maybe replace the initial position character?
  let pos : coords = 
    Array.tryFindIndexJagged ((=)'^') map
    |> Option.get

  let visited = walk Map.empty map pos North

  Map.count visited
  |> Ok

type WalkResult = OutOfBounds | Loop

let rec walk2 (extraBlock: coords) (visited: Set<coords * direction>) (map: char array array) (pos:coords) (dir:direction) : WalkResult =
  let outOfBounds pos = 
    let r, c = pos
    r < 0 || c < 0 || r >= map.Length || c >= map[0].Length

  let isObstructed aim =
    let r, c = aim
    if outOfBounds aim then false
    else map[r][c] = '#' || aim = extraBlock

  if outOfBounds pos then
    OutOfBounds
  elif visited |> Set.contains (pos, dir) then
    Loop
  else
    let r, c = pos
    let aim = 
      match dir with
      | North -> r - 1, c
      | East -> r, c + 1
      | South -> r + 1, c
      | West -> r, c - 1
    if not(isObstructed aim) then
      walk2 extraBlock (Set.add (pos, dir) visited) map aim dir
    else
      let turnedDir = 
        match dir with
        | North -> East
        | East -> South
        | South -> West
        | West -> North
      walk2 extraBlock visited map pos turnedDir

let answer2 (data : parsedInput) =
  let map  = data // maybe replace the initial position character?
  let pos : coords = 
    Array.tryFindIndexJagged ((=)'^') map
    |> Option.get

  let visited = 
    walk Map.empty map pos North
    |> Map.keys
    |> Array.ofSeq
  
  let blockable = 
    visited
    |>Array.Parallel.filter (fun p -> Loop = walk2 p Set.empty map pos North)
  
  Array.length blockable
  |> Ok

type Solver() =
  inherit SolverBase("Guard Gallivant")
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

