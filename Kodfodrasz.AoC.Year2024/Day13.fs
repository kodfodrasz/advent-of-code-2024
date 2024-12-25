module Kodfodrasz.AoC.Year2024.Day13

open System
open System.Text.RegularExpressions
open Kodfodrasz.AoC

open MathNet.Numerics.LinearAlgebra

type Entry =
  { ButtonA : int64 * int64
    ButtonB : int64 * int64
    Prize   : int64 * int64 }

type parsedInput = Entry array

let parseInput (input: string): Result<parsedInput,string> = 
  let matchesButtonA= Regex.Matches(
    input,
    @"Button A: X\+(?<x>\d+), Y\+(?<y>\d+)")
  let matchesButtonB= Regex.Matches(
    input,
    @"Button B: X\+(?<x>\d+), Y\+(?<y>\d+)")
  let matchesPrize = Regex.Matches(
    input,
    @"Prize: X=(?<x>\d+), Y=(?<y>\d+)")
  
  assert (matchesButtonA.Count = matchesButtonB.Count)
  assert (matchesButtonA.Count = matchesPrize.Count)

  let getVals (m : Match) : int64 * int64 = 
    (m.Groups["x"].Value |> int64), (m.Groups["y"].Value |> int64)

  let buttonA = 
    matchesButtonA
    |> Seq.map getVals
  let buttonB = 
    matchesButtonB
    |> Seq.map getVals
  let prize = 
    matchesPrize
    |> Seq.map getVals

  Seq.zip3 buttonA buttonB prize
  |> Seq.map(fun (a, b, p) -> {
      ButtonA = a
      ButtonB = b
      Prize = p
    })
  |> Seq.toArray
  |> Ok

let solve (entry:Entry) : (int64 * int64) option = 
  let v e =
    let x = fst e |> double
    let y = snd e |> double
    vector [x; y]

  let va = v entry.ButtonA
  let vb = v entry.ButtonB
  let vp = v entry.Prize

  let M = DenseMatrix.ofColumns [va; vb]
  let x = M.Solve(vp)
  
  let a = x[0] |> Double.Round |> int64
  let b = x[1] |> Double.Round |> int64

  if (a * fst entry.ButtonA + b * fst entry.ButtonB) = fst entry.Prize
    && (a * snd entry.ButtonA + b * snd entry.ButtonB) = snd entry.Prize
  then Some (a,b)
  else None

let answer1 (data : parsedInput) =
  data
  |> Seq.choose solve
  |> Seq.map (fun (a, b) -> 3L * a + b)
  |> Seq.sum
  |> Ok

let answer2 (data : parsedInput) =
  data
  |> Seq.map(fun e -> 
    let px = 10000000000000L + fst e.Prize
    let py = 10000000000000L + snd e.Prize
    { e with  Prize = px, py }
  )
  |> Seq.choose solve
  |> Seq.map (fun (a, b) -> 3L * a + b)
  |> Seq.sum
  |> Ok
type Solver() =
  inherit SolverBase("Claw Contraption")
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

