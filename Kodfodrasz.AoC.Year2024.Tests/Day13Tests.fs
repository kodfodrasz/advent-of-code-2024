module Kodfodrasz.AoC.Year2024.Tests.Day13Tests

open Xunit
open Swensen.Unquote.Assertions

open Kodfodrasz.AoC.Year2024
open Kodfodrasz.AoC.Year2024.Day13


let exampleInput = """
Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279
"""

[<Fact>]
let ``Parsing example input`` () =
  let expected = [|
    { ButtonA = 94, 34
      ButtonB = 22, 67
      Prize = 8400, 5400 };
    { ButtonA = 26, 66
      ButtonB = 67, 21
      Prize = 12748, 12176 };
    { ButtonA = 17, 86
      ButtonB = 84, 37
      Prize = 7870, 6450 };
    { ButtonA = 69, 23
      ButtonB = 27, 71
      Prize = 18641, 10279 };
  |]

  test
    <@ let actual = parseInput exampleInput
       actual = Ok expected @>

[<Fact>]
let ``Answer 1 helper test 1`` () =
  test <@
    Some (80L, 40L) = solve { 
      ButtonA = 94, 34
      ButtonB = 22, 67
      Prize = 8400, 5400 }
  @>

[<Fact>]
let ``Answer 1 helper test 2`` () =
  test <@
    None = solve     {
      ButtonA = 26, 66
      ButtonB = 67, 21
      Prize = 12748, 12176 }
  @>

[<Fact>]
let ``Answer 1 for example input`` () =
  let input = parseInput exampleInput

  test
    <@ let actual = Result.bind answer1 input
       let expected: Result<_, string> = Ok 480L
       actual = expected @>

[<Fact(Skip="No example result for part 2")>]
let ``Answer 2 for example input`` () =
  let input = parseInput exampleInput

  test
    <@ let actual = Result.bind answer2 input
       let expected: Result<_, string> = Ok 31L
       actual = expected @>
