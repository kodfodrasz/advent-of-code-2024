module Kodfodrasz.AoC.Year2024.Tests.Day3Tests

open Xunit
open Swensen.Unquote.Assertions

open Kodfodrasz.AoC.Year2024
open Kodfodrasz.AoC.Year2024.Day3


let exampleInput = """
xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))
  """

[<Fact>]
let ``Parsing example input`` () =
  let expected: Result<_, string> = 
    Ok "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

  test
    <@ let actual = parseInput exampleInput
       actual = expected @>

[<Fact>]
let ``Answer 1 for example input`` () =
  let input = parseInput exampleInput

  test
    <@ let actual = Result.bind answer1 input
       let expected: Result<int64, string> = Ok 161
       actual = expected @>

let exampleInput2 = """
xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))
"""

[<Fact>]
let ``Answer 2 for example input`` () =
  let input = parseInput exampleInput2

  test
    <@ let actual = Result.bind answer2 input
       let expected: Result<int64, string> = Ok 48
       actual = expected @>
