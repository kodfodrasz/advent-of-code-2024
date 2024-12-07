module Kodfodrasz.AoC.Year2024.Day4

open System
open Kodfodrasz.AoC

type parsedinput = char array array // input[row][column]

let parseInput (input: string): Result<parsedinput, string> =
  input.Split('\n', StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)
  |> Seq.map String.toCharArray
  |> Seq.toArray
  |> Ok

let scanRight (jaggedArr : _ array array) (row, col) = 
  seq {
    let max_col = jaggedArr[0].Length - 1
    for c in col .. max_col do
      yield jaggedArr[row][c]
  }

let scanLeft (jaggedArr : _ array array) (row, col) = 
  seq {
    for c in col .. -1 .. 0 do
      yield jaggedArr[row][c]
  }

let scanUp (jaggedArr : char array array) (row, col) = 
  seq {
    for r in row .. -1 .. 0 do
      yield jaggedArr[r][col]
  }

let scanDown (jaggedArr : _ array array) (row, col) = 
  seq {
    let max_row = jaggedArr.Length - 1
    for r in row .. max_row do
      yield jaggedArr[r][col]
  }

let scanDiagonalUpLeft (jaggedArr : _ array array) (row, col) = 
  seq {
    for (r, c) in (Seq.zip (seq { row .. -1 .. 0 }) (seq { col .. -1 .. 0 })) do
      yield jaggedArr[r][c]
  }

let scanDiagonalUpRight (jaggedArr : _ array array) (row, col) = 
  let max_col = jaggedArr[0].Length - 1
  seq {
    for (r, c) in (Seq.zip (seq { row .. -1 .. 0 }) (seq { col .. max_col })) do
      yield jaggedArr[r][c]
  }

let scanDiagonalDownLeft (jaggedArr : _ array array) (row, col) = 
  let max_row = jaggedArr.Length - 1
  seq {
    for (r, c) in (Seq.zip (seq { row .. max_row }) (seq { col .. -1 .. 0 })) do
      yield jaggedArr[r][c]
  }

let scanDiagonalDownRight (jaggedArr : _ array array) (row, col) : char seq = 
  let max_row = jaggedArr.Length - 1
  let max_col = jaggedArr[0].Length - 1
  seq {
    for (r, c) in (Seq.zip (seq { row .. max_row }) (seq { col .. max_col })) do
      yield jaggedArr[r][c]
  }

let toWord (s : char seq) = 
  s |> Seq.truncate 4 |> Seq.toArray |> string

let isXMAS (s : char seq) = 
  let chars = s |> Seq.truncate 4 |> Seq.toArray
  chars = [| 'X';'M';'A';'S'; |]

let answer1 (input : parsedinput) : Result<int,string> =
  let indices = seq { 
    let max_row = input.Length - 1 
    let max_col = input[0].Length - 1
    
    for r in 0 .. max_row do
      for c in 0 .. max_col do
      yield r, c
  }

  let checkers = [|
    (scanUp input >> isXMAS);
    (scanDown input >> isXMAS);
    (scanLeft input >> isXMAS);
    (scanRight input >> isXMAS);
    (scanDiagonalUpLeft input >> isXMAS);
    (scanDiagonalUpRight input >> isXMAS);
    (scanDiagonalDownLeft input >> isXMAS);
    (scanDiagonalDownRight input >> isXMAS);
  |]

  let checkIdx (r, c) checker =
    (r, c) , (checker (r, c))

  let matches = 
    indices
    |> Seq.collect( fun (r, c) -> 
      let results = (Array.Parallel.map (checkIdx (r, c)) checkers)
      results)
    |> Seq.where snd
    |> Seq.toArray

  matches
  |> Seq.length
  |> Ok

let answer2 (input : parsedinput) : Result<int,string>  =
  failwith "TODO"

type Solver() =
  inherit SolverBase("Ceres Search")
  with
    override this.Solve input =
      this.DoSolve parseInput [ answer1; answer2 ] input

