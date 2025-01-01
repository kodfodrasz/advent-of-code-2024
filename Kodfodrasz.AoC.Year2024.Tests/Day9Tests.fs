module Kodfodrasz.AoC.Year2024.Tests.Day9Tests

open Xunit
open Swensen.Unquote.Assertions

open Kodfodrasz.AoC
open Kodfodrasz.AoC.Year2024
open Kodfodrasz.AoC.Year2024.Day9


let exampleInput = """
2333133121414131402
  """

[<Fact>]
let ``Parsing example input`` () =
  let expected = [|
    File (0, 2, 0)
    Free (2, 3)
    File (5, 3, 1)
    Free (8, 3)
    File (11, 1, 2)
    Free (12, 3)
    File (15, 3, 3)
    Free (18, 1)
    File (19, 2, 4)
    Free (21, 1)
    File (22, 4, 5)
    Free (26, 1)
    File (27, 4, 6)
    Free (31, 1)
    File (32, 3, 7)
    Free (35, 1)
    File (36, 4, 8)
    Free (40, 0)
    File (40, 2, 9)
  |]

  test
    <@ let actual = parseInput exampleInput
       actual = Ok expected @>

[<Fact>]
let ``Answer 1 for example input`` () =
  let input = parseInput exampleInput

  test
    <@ let actual = Result.bind answer1 input
       let expected: Result<_, string> = Ok 1928L
       actual = expected @>

[<Fact(Skip="TODO")>]
let ``Answer 2 for example input`` () =
  let input = parseInput exampleInput

  test
    <@ let actual = Result.bind answer2 input
       let expected: Result<_, string> = Ok 31L
       actual = expected @>
