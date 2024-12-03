module Kodfodrasz.AoC.Year2024.Tests.Day1Tests

open Xunit
open Swensen.Unquote.Assertions

open Kodfodrasz.AoC.Year2024
open Kodfodrasz.AoC.Year2024.Day1


let exampleInput = """
TODO
  """

[<Fact(Skip="TODO")>]
let ``Parsing example input`` () =
  let expected: Result<string list, string> = Ok [ 
    "TODO";
  ]

  test
    <@ let actual = parseInput exampleInput
       actual = expected @>

[<Fact(Skip="TODO")>]
let ``Answer 1 for example input`` () =
  let input = parseInput exampleInput

  test
    <@ let actual = Result.bind answer1 input
       let expected: Result<int, string> = Ok 142
       actual = expected @>
