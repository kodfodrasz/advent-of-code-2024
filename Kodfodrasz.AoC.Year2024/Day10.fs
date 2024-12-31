module Kodfodrasz.AoC.Year2024.Day10

open System
open System.Text.RegularExpressions
open Kodfodrasz.AoC

type parsedInput = int array array
let parseInput (input: string): Result<parsedInput,string> = 
  input.Split('\n', StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)
  |> Seq.map (String.toCharArray >> Array.map (fun c -> 
    if Char.IsDigit(c) then
      int c - int '0'
    else
      -1
    )
  )
  |> Seq.toArray
  |> Ok

type coords = int * int

let steps i j (arr : _ array2d) =
  seq {
    if (i > 0) then yield i-1, j
    if (j < Array2D.length2 arr - 1) then yield i, j+1
    if (i < Array2D.length1 arr - 1) then yield i+1, j
    if (j > 0) then yield i, j-1
  }
  |> Seq.toList

//let rec walkCount (map : int array2d) (pos : coords) = 
//  let i, j = pos
//  let curr = map[i,j]
//  if curr = 9 then
//    1
//  else
//    let neighbours = 
//      steps i j map
//      |> List.map (fun (i, j) -> (i, j), map[i, j])
//    let good = 
//      neighbours
//      |> List.filter(fun (p, v) -> v = curr + 1)

//    good
//    |> List.sumBy(fun (p, v) -> walkCount map p)


let answer1 (data : parsedInput) =
  let map = array2D data
  let zeroes = 
    map
    |> Array2D.foldi (fun i j l n -> if n = 0 then (i,j) :: l else l ) List.empty
    |> List.rev
    |> List.toArray

  let rec walkCount (map : int array2d) (acc:coords list)  (pos : coords) = 
    let i, j = pos
    let curr = map[i,j]
    if curr = 9 then
      (i, j) :: acc
    else
      let neighbours = 
        steps i j map
        |> List.map (fun (i, j) -> (i, j), map[i, j])
      let good = 
        neighbours
        |> List.filter(fun (p, v) -> v = curr + 1)

      good
      |> List.map fst
      |> List.fold (walkCount map) acc

  zeroes
  |> Array.Parallel.sumBy (walkCount map List.empty >> Seq.distinct >> Seq.length)
  |> Ok


let answer2 (data : parsedInput) =
  failwith "TODO"

type Solver() =
  inherit SolverBase("Hoof It")
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

