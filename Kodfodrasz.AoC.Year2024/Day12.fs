module Kodfodrasz.AoC.Year2024.Day12

open System
open System.Text.RegularExpressions
open Kodfodrasz.AoC

type parsedInput = char array2d
let parseInput (input: string): Result<parsedInput,string> = 
  input.Split('\n', StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)
  |> Seq.map (String.toCharArray)
  |> array2D
  |> Ok

let steps i j (arr : _ array2d) =
  seq {
    if (i > 0) then yield i-1, j
    if (j < Array2D.length2 arr - 1) then yield i, j+1
    if (i < Array2D.length1 arr - 1) then yield i+1, j
    if (j > 0) then yield i, j-1
  }
  |> Seq.toList

type coords = int * int
type region = {
  Kind : char
  Pos : coords list
}
let rec walk 
  (map : parsedInput)
  (acc: region list)
  (pending  : coords list)
  =
    match pending with
    | [] -> acc
    | pos :: still_pending ->
      let i, j = pos
      let plot = map[i,j]
      // ensure identical items are always at the head of the list!
      // the list has a limited length, max 4 items
      let neighbours =
        steps i j map
      let frontier = 
          let is_samekind ((ci,cj) : coords)  = map[ci,cj] = plot
          let not_samekind ((ci,cj) : coords)  = map[ci,cj] <> plot
          let n_same = neighbours |> Seq.filter is_samekind

          Seq.concat [ 
            still_pending |> Seq.filter is_samekind
            neighbours |> Seq.filter is_samekind
            neighbours |> Seq.filter not_samekind
            still_pending |> Seq.filter not_samekind
          ]
          |> Seq.toList
      match acc with
      | head :: tail when head.Kind = plot ->
        let region =
            { head with
                Pos = (i,j) :: head.Pos
            }
        walk map (region :: tail) frontier
      | _ -> 
        let region =
          {
            Kind = plot
            Pos = [ i,j ]
          }
        walk map (region :: acc) frontier

let answer1 (data : parsedInput) =
  let regions =
    walk data [] [0,0]

  failwith "TODO"

let answer2 (data : parsedInput) =
  failwith "TODO"

type Solver() =
  inherit SolverBase("Garden Groups")
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

