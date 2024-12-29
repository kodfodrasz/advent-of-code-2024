module Kodfodrasz.AoC.Year2024.Tests.Day5Tests

open Xunit
open Swensen.Unquote.Assertions

open Kodfodrasz.AoC
open Kodfodrasz.AoC.Year2024
open Kodfodrasz.AoC.Year2024.Day5


let exampleInput = """
47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47
  """

[<Fact>]
let ``Parsing example input`` () =
  let expected = 
    {
        Rules= [
          { Before = 47; After = 53 }
          { Before = 97; After = 13 }
          { Before = 97; After = 61 }
          { Before = 97; After = 47 }
          { Before = 75; After = 29 }
          { Before = 61; After = 13 }
          { Before = 75; After = 53 }
          { Before = 29; After = 13 }
          { Before = 97; After = 29 }
          { Before = 53; After = 29 }
          { Before = 61; After = 53 }
          { Before = 97; After = 53 }
          { Before = 61; After = 29 }
          { Before = 47; After = 13 }
          { Before = 75; After = 47 }
          { Before = 97; After = 75 }
          { Before = 47; After = 61 }
          { Before = 75; After = 61 }
          { Before = 47; After = 29 }
          { Before = 75; After = 13 }
          { Before = 53; After = 13 }
        ]
        Batches= [
          [75; 47; 61; 53; 29]
          [97; 61; 53; 29; 13]
          [75; 29; 13]
          [75; 97; 47; 61; 53]
          [61; 13; 29]
          [97; 13; 75; 29; 47]
        ]
       }

  test
    <@ let actual = parseInput exampleInput
       actual = Ok expected @>

[<Fact>]
let ``Answer 1 for example input`` () =
  let input = parseInput exampleInput

  test
    <@ let actual = Result.bind answer1 input
       let expected: Result<_, string> = Ok 143L
       actual = expected @>

[<Fact>]
let ``Answer 1 helper function: check`` () =
  let input = parseInput exampleInput |> Result.get

  let rulesByPreceding = 
    input.Rules
    |> Seq.groupBy (fun r -> r.Before)
    |> Map.ofSeq
    |> Map.map (fun b r -> r |> Seq.map (fun rr -> rr.After) |> Seq.toArray |> Array.sort)

  let rulesByFollowing = 
    input.Rules
    |> Seq.groupBy (fun r -> r.After)
    |> Map.ofSeq
    |> Map.map (fun b r -> r |> Seq.map (fun rr -> rr.Before) |> Seq.toArray |> Array.sort)

  test <@ true = Day5.check rulesByPreceding rulesByFollowing [75;47;61;53;29] @>
  test <@ true = Day5.check rulesByPreceding rulesByFollowing [97;61;53;29;13] @>
  test <@ true = Day5.check rulesByPreceding rulesByFollowing [75;29;13] @>

  test <@ false = Day5.check rulesByPreceding rulesByFollowing [75;97;47;61;53] @>
  test <@ false = Day5.check rulesByPreceding rulesByFollowing [61;13;29] @>
  test <@ false = Day5.check rulesByPreceding rulesByFollowing [97;13;75;29;47] @>



[<Fact(Skip="TODO")>]
let ``Answer 2 for example input`` () =
  let input = parseInput exampleInput

  test
    <@ let actual = Result.bind answer2 input
       let expected: Result<_, string> = Ok 31
       actual = expected @>
