module Kodfodrasz.AoC.Year2024.Day22

open System
open System.Text.RegularExpressions
open Kodfodrasz.AoC

// TODO: move to library?
let uncurry f (x, y) = f x y


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

let answer2_params (len) (data : parsedInput) =
  let series = 
    data
    |> Array.Parallel.map (
      Seq.unfold (fun secret -> Some (secret, evolve secret)) 
      >> Seq.take (len + 1)
      >> Seq.toArray
    )
  
  let toPrice i = i % 10

  let prices =
    series
    |> Array.Parallel.map (Array.map toPrice)

  let deltas =
    prices
    |> Array.Parallel.map (
      Array.pairwise
      >> Array.map (uncurry (-))
    )

  let priceWithChanges = 
    deltas
    |> Array.mapi(fun i d ->
        let c = d |> Array.windowed 4
        let p = prices[i] |> Array.skip 4
        assert (c.Length = p.Length)
        Array.zip c p
    )

  let allSequences =
    priceWithChanges
    |> Seq.collect (Array.map fst)
    |> Seq.distinct
    |> Seq.toArray

  let allResults = 
    allSequences
    |> Array.Parallel.map (fun seq -> 
      priceWithChanges // all seller, seq win 4 * price
      |> Array.choose (Array.tryFind(fun (s,p) -> s = seq) >> Option.map snd)
      |> Array.sum
    )

  allResults
    |> Array.max
    |> int64
    |>Ok

let answer2 = answer2_params 2001

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

