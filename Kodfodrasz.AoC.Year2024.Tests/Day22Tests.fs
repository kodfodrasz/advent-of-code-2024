module Kodfodrasz.AoC.Year2024.Tests.Day22Tests

open Xunit
open Swensen.Unquote.Assertions

open Kodfodrasz.AoC.Year2024
open Kodfodrasz.AoC.Year2024.Day22


let exampleInput = """
1
10
100
2024
  """

[<Fact>]
let ``Parsing example input`` () =
  let expected = [| 
    1;
    10;
    100;
    2024;
  |]

  test
    <@ let actual = parseInput exampleInput
       actual = Ok expected @>

[<Fact>]
let ``Answer 1 for example input`` () =
  let input = parseInput exampleInput

  test
    <@ let actual = Result.bind answer1 input
       let expected: Result<_, string> = Ok 37327623L
       actual = expected @>

[<Fact>]
let ``Answer 2 for example input`` () =
  let input = parseInput """
1
2
3
2024
  """

  test
    <@ let actual = Result.bind (answer2_params 2000) input
       let expected: Result<_, string> = Ok 23L
       actual = expected @>
