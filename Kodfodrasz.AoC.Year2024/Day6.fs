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

let answer1 (data : parsedInput) =
  let rec walk (visited : Set<coords>) (map : char array array) (pos:coords) (dir:direction) : Set<coords> =
    let outOfBounds pos = 
      let r, c = pos
      r < 0 || c < 0 || r >= map.Length || r >= map[0].Length

    let isObstructed aim =
      let r, c = aim
      if outOfBounds aim then false
      else map[r][c] = '#'

    if outOfBounds pos then
      Set.remove pos visited
    else
      let r, c = pos
      let aim = 
        match dir with
        | North -> r - 1, c
        | East -> r, c + 1
        | South -> r + 1, c
        | West -> r, c - 1
      if not(isObstructed aim) then
        walk (Set.add pos visited) map aim dir
      else
        let turnedDir = 
          match dir with
          | North -> East
          | East -> South
          | South -> West
          | West -> North
        walk visited map pos turnedDir

  let map  = data // maybe replace the initial position character?
  let pos : coords = 
    Array.tryFindIndexJagged ((=)'^') map
    |> Option.get

  let visited = walk Set.empty map pos North

  Set.count visited
  |> Ok

let answer2 (data : parsedInput) =
  failwith "TODO"

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

