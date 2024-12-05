module Kodfodrasz.AoC.Year2024.Tests.Day2Tests

open Xunit
open Swensen.Unquote.Assertions

open Kodfodrasz.AoC.Year2024
open Kodfodrasz.AoC.Year2024.Day2


let exampleInput = """
7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9
  """

[<Fact>]
let ``Parsing example input`` () =
  let expected: Result<_, string> = Ok [| 
      [| 7; 6; 4; 2; 1; |]
      [| 1; 2; 7; 8; 9; |]
      [| 9; 7; 6; 2; 1; |]
      [| 1; 3; 2; 4; 5; |]
      [| 8; 6; 4; 4; 1; |]
      [| 1; 3; 6; 7; 9; |]
  |]

  test
    <@ let actual = parseInput exampleInput
       actual = expected @>

[<Fact>]
let ``Answer 1 for example input`` () =
  let input = parseInput exampleInput

  test
    <@ let actual = Result.bind answer1 input
       let expected: Result<int, string> = Ok 2
       actual = expected @>

[<Fact(Skip="TODO")>]
let ``Answer 2 for example input`` () =
  let input = parseInput exampleInput

  test
    <@ let actual = Result.bind answer2 input
       let expected: Result<int, string> = Ok 31
       actual = expected @>
