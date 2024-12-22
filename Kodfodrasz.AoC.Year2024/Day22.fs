module Kodfodrasz.AoC.Year2024.Day22

open System
open System.Text.RegularExpressions
open Kodfodrasz.AoC

type parsedInput = int array
let parseInput (input: string): Result<parsedInput,string> = 
    input
  |> String.splitTrimAndRemoveEmpty [| '\n' |]
  |> Array.map int
  |> Ok

let evolve (secret:int) = 
    let mutable a = secret
    a <- a ^^^ (a <<< 6)
    a <- a &&& 0xFFFFFF
    a <- a ^^^ (a >>> 5)
    a <- a &&& 0xFFFFFF
    a <- a ^^^ (a <<< 11)
    a &&& 0xFFFFFF

let rec iter i secret =
    match i with
    | 0 ->  secret
    | _ -> iter (i - 1) (evolve secret)
;;

let answer1 (data : parsedInput) =
  data
  |> Array.Parallel.map (iter 2000 >> int64)
  |> Array.Parallel.sum
  |> Ok

let answer2 (data : parsedInput) =
  failwith "TODO"

type Solver() =
  inherit SolverBase("Monkey Market")
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

