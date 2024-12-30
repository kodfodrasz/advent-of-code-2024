module Kodfodrasz.AoC.Year2024.Tests.Day6Tests

open Xunit
open Swensen.Unquote.Assertions

open Kodfodrasz.AoC
open Kodfodrasz.AoC.Year2024
open Kodfodrasz.AoC.Year2024.Day6


let exampleInput = """
....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...
  """

[<Fact>]
let ``Parsing example input`` () =
  let expected = 
    [|
      [|'.'; '.'; '.'; '.'; '#'; '.'; '.'; '.'; '.'; '.'|];
      [|'.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '#'|];
      [|'.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'|];
      [|'.'; '.'; '#'; '.'; '.'; '.'; '.'; '.'; '.'; '.'|];
      [|'.'; '.'; '.'; '.'; '.'; '.'; '.'; '#'; '.'; '.'|];
      [|'.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'|];
      [|'.'; '#'; '.'; '.'; '^'; '.'; '.'; '.'; '.'; '.'|];
      [|'.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '#'; '.'|];
      [|'#'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'|];
      [|'.'; '.'; '.'; '.'; '.'; '.'; '#'; '.'; '.'; '.'|];
    |]

  test
    <@ let actual = parseInput exampleInput
       actual = Ok expected @>

[<Fact>]
let ``Answer 1 for example input`` () =
  let input = parseInput exampleInput

  test
    <@ let actual = Result.bind answer1 input
       let expected: Result<_, string> = Ok 41
       actual = expected @>

[<Fact>]
let ``Answer 2 for example input`` () =
  let input = parseInput exampleInput

  test
    <@ let actual = Result.bind answer2 input
       let expected: Result<_, string> = Ok 6
       actual = expected @>
