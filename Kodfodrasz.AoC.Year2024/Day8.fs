module Kodfodrasz.AoC.Year2024.Day8

open System
open System.Text.RegularExpressions
open Kodfodrasz.AoC

type parsedInput = char array2d
let parseInput (input: string): Result<parsedInput,string> = 
  input.Split('\n', StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)
  |> Seq.map (String.toCharArray)
  |> array2D
  |> Ok

type coords = int * int

let antipodes (map : _ array2d) (l : coords list) =
  let pairings =
    List.allPairs l l
    |> List.filter (fun (a,b) -> a <> b)

  pairings
  |> List.collect(fun (a, b) ->
    let (a_r, a_c) = a
    let (b_r, b_c) = b
    let (d_r, d_c) = (b_r - a_r, b_c - a_c)

    [ (a_r - d_r, a_c - d_c); (b_r + d_r, b_c + d_c) ]
  )
  |> List.filter(fun (r,c) -> 
    (map |> Array2D.base1 <= r)
    && (map |> Array2D.base2 <=c)
    && (map |> Array2D.length1 > r)
    && (map |> Array2D.length2 > c) 
  )

let answer1 (map : parsedInput) =
  let zeroes = 
    map
    |> Array2D.foldi (fun i j l c -> 
      if Char.IsAsciiLetterOrDigit c then (i,j) :: l
      else l 
    ) List.empty
    |> List.groupBy (fun (i,j) -> map[i,j])
    |> Map.ofList

  let antipodes = 
    zeroes
    |> Map.values
    |> Seq.collect (antipodes map)
    |> Seq.distinct
    |> Seq.toArray

  antipodes |> Array.length |> Ok

let answer2 (data : parsedInput) =
  failwith "TODO"

type Solver() =
  inherit SolverBase("Resonant Collinearity")
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

