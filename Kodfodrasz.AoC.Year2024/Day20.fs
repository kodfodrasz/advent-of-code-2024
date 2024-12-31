module Kodfodrasz.AoC.Year2024.Day20

open System
open System.Text.RegularExpressions
open Kodfodrasz.AoC
open MathNet.Numerics
open MathNet.Numerics.LinearAlgebra

type parsedInput = char array array

let parseInput (input : string) : Result<parsedInput,string> = 
  input
  |> String.splitTrimAndRemoveEmpty [| '\n' |]
  |> Array.map String.toCharArray
  |> Ok

let tryFindIndex predicate  (arr: _ array array)  =
    arr
    |> Array.mapi (fun i row   -> 
          match Array.tryFindIndex predicate row with
          | Some j -> Some (i, j)
          | _ -> None)
    |> Seq.choose id
    |> Seq.tryHead

let steps i j (arr : _ array array) =
  seq {
    if (i > 0) then yield i-1, j
    if (j < Array.length arr[i] - 1) then yield i, j+1
    if (i < Array.length arr - 1) then yield i+1, j
    if (j > 0) then yield i, j-1
  }

let cheats i j (arr : char array array) =
  let isWall c  = c = '#'
  let isStep c = (c = '.' || c = 'E')
  seq {
    if (i > 1) && arr[i-1][j]|> isWall && arr[i-2][j] |> isStep then
      // yield i-1, j
      yield i-2, j
    if (j < Array.length arr[i] - 3) && arr[i][j+1]|> isWall && arr[i][j+2] |> isStep then
      // yield i, j+1
      yield i, j+2
    if (i < Array.length arr - 3) && arr[i+1][j]|> isWall && arr[i+2][j] |> isStep then
      // yield i+1, j
      yield i+2, j
    if (j > 1) && arr[i][j-1]|> isWall && arr[i][j-2] |> isStep then
      // yield i, j-1
      yield i, j-2
  }

let answer1_params limit (data : parsedInput) =
  let start = data |> tryFindIndex ((=)'S') |> Option.get
  let finish = data |> tryFindIndex ((=)'E') |> Option.get

  let rec walk path (i,j) =
    let c = data[i][j]
    match c with
    | 'E' -> (i,j) :: path |> List.rev |> List.toArray
    | '.' | 'S' -> 
      let next = 
        data 
        |> steps i j 
        |> Seq.where (fun (ii,jj) -> 
            let s = data[ii][jj]
            s = 'S' || s = '.' || s = 'E')
        |> Seq.except (Seq.truncate 1 path) 
        |> Seq.exactlyOne
      walk ((i,j) :: path) next
    | _ -> failwith "Invalid position, should never happen"

  let path = walk [] start

  assert (finish = (Array.last path))

  let distances = 
    path
    |> Array.mapi (fun idx (row, col) -> (row, col), (Array.length path - idx - 1))
    |> Map.ofArray

  let cheat_savings =
    distances.Keys
    |> Seq.collect (fun (i, j) ->
        data
        |> cheats i j
        |> Seq.map (fun (k, l) -> 
          let f = distances |> Map.find (i, j)
          let t = distances |> Map.find (k, l)
          let saved = f - t - 2
          saved
          )
    )
    |> Seq.groupBy id
    |> Map.ofSeq
    |> Map.map (fun _ s -> Seq.length s)

  cheat_savings
  |> Map.filter (fun k v -> k >= limit)
  |> Map.values
  |> Seq.sum
  |> Ok

let answer1 = answer1_params 100

let answer2_params radius threshold (data : parsedInput) =
  let start = data |> tryFindIndex ((=)'S') |> Option.get
  let finish = data |> tryFindIndex ((=)'E') |> Option.get

  let rec walk path (i,j) =
    let c = data[i][j]
    match c with
    | 'E' -> (i,j) :: path |> List.rev |> List.toArray
    | '.' | 'S' -> 
      let next = 
        data 
        |> steps i j 
        |> Seq.where (fun (ii,jj) -> 
            let s = data[ii][jj]
            s = 'S' || s = '.' || s = 'E')
        |> Seq.except (Seq.truncate 1 path) 
        |> Seq.exactlyOne
      walk ((i,j) :: path) next
    | _ -> failwith "Invalid position, should never happen"

  let path = walk [] start

  assert (finish = (Array.last path))

  let distances = 
    path
    |> Array.mapi (fun idx (row, col) -> (row, col), (Array.length path - idx - 1))
    |> Map.ofArray

  let cheat_savings =
    distances.Keys
    |> Seq.collect (fun (i, j) ->
        distances
        |> Map.filter (fun (k, l) v -> 
          let md = int <| Distance.Manhattan (
            vector [double i; double j],
            vector [double k; double l])
          md <= radius
        )
        |> Map.map (fun (k, l) d -> 
          let md = int <| Distance.Manhattan (
            vector [double i; double j],
            vector [double k; double l])
          let f = distances |> Map.find (i, j)
          let t = distances |> Map.find (k, l)
          let saved = f - t - md
          saved
        )
        |> Map.values
    )
    |> Seq.groupBy id
    |> Map.ofSeq
    |> Map.map (fun _ s -> s |> Seq.length)

  cheat_savings
  |> Map.filter (fun k v -> k >= threshold)
  |> Map.values
  |> Seq.sum
  |> Ok

let answer2 = answer2_params 20 100

type Solver() =
  inherit SolverBase("Race Condition")
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

