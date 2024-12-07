module Kodfodrasz.AoC.Year2024.Tests.Day4Tests

open Xunit
open Swensen.Unquote.Assertions

open Kodfodrasz.AoC.Year2024
open Kodfodrasz.AoC.Year2024.Day4


let exampleInput = """
MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX
  """

[<Fact>]
let ``Parsing example input`` () =
  let expected: Result<parsedinput, string> = Ok [|
      [|'M'; 'M'; 'M'; 'S'; 'X'; 'X'; 'M'; 'A'; 'S'; 'M'; |];
      [|'M'; 'S'; 'A'; 'M'; 'X'; 'M'; 'S'; 'M'; 'S'; 'A'; |];
      [|'A'; 'M'; 'X'; 'S'; 'X'; 'M'; 'A'; 'A'; 'M'; 'M'; |];
      [|'M'; 'S'; 'A'; 'M'; 'A'; 'S'; 'M'; 'S'; 'M'; 'X'; |];
      [|'X'; 'M'; 'A'; 'S'; 'A'; 'M'; 'X'; 'A'; 'M'; 'M'; |];
      [|'X'; 'X'; 'A'; 'M'; 'M'; 'X'; 'X'; 'A'; 'M'; 'A'; |];
      [|'S'; 'M'; 'S'; 'M'; 'S'; 'A'; 'S'; 'X'; 'S'; 'S'; |];
      [|'S'; 'A'; 'X'; 'A'; 'M'; 'A'; 'S'; 'A'; 'A'; 'A'; |];
      [|'M'; 'A'; 'M'; 'M'; 'M'; 'X'; 'M'; 'M'; 'M'; 'M'; |];
      [|'M'; 'X'; 'M'; 'X'; 'A'; 'X'; 'M'; 'A'; 'S'; 'X'; |];
  |]

  test
    <@ let actual = parseInput exampleInput
       actual = expected @>

[<Fact>]
let ``Answer 1 for example input`` () =
  let input = parseInput exampleInput

  test
    <@ let actual = Result.bind answer1 input
       let expected: Result<int, string> = Ok 18
       actual = expected @>

[<Fact>]
let ``Answer 2 for example input`` () =
  let input = parseInput exampleInput

  test
    <@ let actual = Result.bind answer2 input
       let expected: Result<int, string> = Ok 9
       actual = expected @>


[<Fact>]
let ``Answer 1 debug (0,4) scanDiagonalDownRight`` () =
  let input = parseInput exampleInput |> Result.defaultValue null

  test
    <@ let actual = scanDiagonalDownRight input (0,4) |> isXMAS
       actual = true @>

[<Fact>]
let ``Answer 2 debug (1,2) isXMAS2`` () =
  let input = parseInput exampleInput |> Result.defaultValue null

  test
    <@ let actual = isXMAS2 input (1,2)
       actual = true @>
