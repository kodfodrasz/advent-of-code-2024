module Kodfodrasz.AoC.Year2024.Day13

open System
open System.Text.RegularExpressions
open Kodfodrasz.AoC

open MathNet.Numerics.LinearAlgebra

type Entry =
  { ButtonA : int*int
    ButtonB : int*int
    Prize   : int*int }

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

  let getVals (m : Match) : int*int = 
    (m.Groups["x"].Value |> int), (m.Groups["y"].Value |> int)

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

let solve (entry:Entry) : (int*int) option = 
  let v e =
    let x = fst e |> double
    let y = snd e |> double
    vector [x; y]

  let va = v entry.ButtonA
  let vb = v entry.ButtonB
  let vp = v entry.Prize

  let M = DenseMatrix.ofColumns [va; vb]
  let x = M.Solve(vp)
  
  let a = x[0] |> Double.Round |> int 
  let b = x[1] |> Double.Round |> int

  if (a * fst entry.ButtonA + b * fst entry.ButtonB) = fst entry.Prize
    && (a * snd entry.ButtonA + b * snd entry.ButtonB) = snd entry.Prize
  then Some (a,b)
  else None

let answer1 (data : parsedInput) =
  data
  |> Seq.choose solve
  |> Seq.map (fun (a, b) -> 3 * a + b)
  |> Seq.sum
  |> Ok

let answer2 (data : parsedInput) =
  failwith "TODO"

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

