module Kodfodrasz.AoC.Year2024.Day5

open System
open System.Text.RegularExpressions
open Kodfodrasz.AoC

type OrderingRule = 
  { Before : int
    After  : int }

type PrintQueue = 
  { Rules : OrderingRule list
    Batches : int list list }

type parsedInput = PrintQueue
let parseInput (input: string): Result<parsedInput,string> = 
  let matchesOrderingRule= Regex.Matches(
    input,
    @"^\s*(?<before>\d+)\|(?<after>\d+)\s*$",
    RegexOptions.Multiline)
  let matchesBatches= Regex.Matches(
    input,
    @"^\s*(?:(?<n>\d+),?)+\s*$",
    RegexOptions.Multiline)

  let orderingRules = 
    let parseRule (m:Match) = 
      let b = m.Groups["before"].Value |> int
      let a = m.Groups["after"].Value |> int
      {
        Before = b
        After = a
      }

    matchesOrderingRule
    |> Seq.map parseRule
    |> Seq.toList

  let batches = 
    let parseMatch (m:Match) = 
      m.Groups["n"].Captures 
      |> Seq.map (fun (c:Capture) -> c.Value |> int)
      |> Seq.toList

    matchesBatches
    |> Seq.map parseMatch
    |> Seq.toList

  Ok {
    Rules = orderingRules
    Batches = batches
  }

let rec check (rulesByPreceding:Map<int,int array>) (rulesByFollowing:Map<int,int array>) (l:int list) = 
  match l with
  | [] // for complete match
  | _ :: [] -> true // stop on single remaining item, if it didn't violate anything until this point, then it is fine
  | head :: tail -> 
    let violatesPrecedingRules = 
      match Map.tryFind head rulesByPreceding with
      | Some rules ->
        tail 
        |> List.exists (fun p -> Array.BinarySearch(rules, p) < 0)
      | None -> false

    let violatesFollowingRules = 
      match Map.tryFind head rulesByFollowing with
      | Some rules ->
        tail 
        |> List.forall (fun p -> Array.BinarySearch(rules, p) < 0)
        |> not
      | None -> false

    if violatesPrecedingRules || violatesFollowingRules then false
    else check rulesByPreceding rulesByFollowing tail


let answer1 (data : parsedInput) =
  let rulesByPreceding = 
    data.Rules
    |> Seq.groupBy (fun r -> r.Before)
    |> Map.ofSeq
    |> Map.map (fun b r -> r |> Seq.map (fun rr -> rr.After) |> Seq.toArray |> Array.sort)

  let rulesByFollowing = 
    data.Rules
    |> Seq.groupBy (fun r -> r.After)
    |> Map.ofSeq
    |> Map.map (fun b r -> r |> Seq.map (fun rr -> rr.Before) |> Seq.toArray |> Array.sort)


  let middlePage (b:int list) = 
    let l = List.length b
    b[l / 2] |> int64

  let middlePages =
    data.Batches
    |> Seq.filter (check rulesByPreceding rulesByFollowing)
    |> Seq.map middlePage
  
  middlePages
  |> Seq.sum
  |> Ok

let answer2 (data : parsedInput) =
  failwith "TODO"

type Solver() =
  inherit SolverBase("Print Queue")
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

