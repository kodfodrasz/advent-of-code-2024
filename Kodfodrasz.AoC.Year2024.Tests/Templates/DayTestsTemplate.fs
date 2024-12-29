module private Kodfodrasz.AoC.Year2024.Tests.DayXTests

open Xunit
open Swensen.Unquote.Assertions

open Kodfodrasz.AoC
open Kodfodrasz.AoC.Year2024
open Kodfodrasz.AoC.Year2024.DayX


let exampleInput = """
1
2
3
4
  """

[<Fact(Skip="This is a template")>]
let ``Parsing example input`` () =
  let expected = [ 1; 2; 3; 4; ]

  test
    <@ let actual = parseInput exampleInput
       actual = Ok expected @>

[<Fact(Skip="This is a template")>]
let ``Answer 1 for example input`` () =
  let input = parseInput exampleInput

  test
    <@ let actual = Result.bind answer1 input
       let expected: Result<_, string> = Ok 11
       actual = expected @>

[<Fact(Skip="This is a template")>]
let ``Answer 2 for example input`` () =
  let input = parseInput exampleInput

  test
    <@ let actual = Result.bind answer2 input
       let expected: Result<_, string> = Ok 31
       actual = expected @>
