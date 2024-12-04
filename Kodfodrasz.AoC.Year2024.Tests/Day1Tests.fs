module Kodfodrasz.AoC.Year2024.Tests.Day1Tests

open Xunit
open Swensen.Unquote.Assertions

open Kodfodrasz.AoC.Year2024
open Kodfodrasz.AoC.Year2024.Day1


let exampleInput = """
3   4
4   3
2   5
1   3
3   9
3   3
  """

[<Fact>]
let ``Parsing example input`` () =
  let expected: Result<(int*int) list, string> = Ok [ 
      3, 4;
      4, 3;
      2, 5;
      1, 3;
      3, 9;
      3, 3;
  ]

  test
    <@ let actual = parseInput exampleInput
       actual = expected @>

[<Fact>]
let ``Answer 1 for example input`` () =
  let input = parseInput exampleInput

  test
    <@ let actual = Result.bind answer1 input
       let expected: Result<int, string> = Ok 11
       actual = expected @>

[<Fact>]
let ``Answer 2 for example input`` () =
  let input = parseInput exampleInput

  test
    <@ let actual = Result.bind answer2 input
       let expected: Result<int, string> = Ok 31
       actual = expected @>
