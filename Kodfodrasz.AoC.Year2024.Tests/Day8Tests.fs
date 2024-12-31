module Kodfodrasz.AoC.Year2024.Tests.Day8Tests

open Xunit
open Swensen.Unquote.Assertions

open Kodfodrasz.AoC
open Kodfodrasz.AoC.Year2024
open Kodfodrasz.AoC.Year2024.Day8


let exampleInput = """
............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............
"""

[<Fact>]
let ``Parsing example input`` () =
  let expected = array2D [
    [ '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; ]
    [ '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '0'; '.'; '.'; '.'; ]
    [ '.'; '.'; '.'; '.'; '.'; '0'; '.'; '.'; '.'; '.'; '.'; '.'; ]
    [ '.'; '.'; '.'; '.'; '.'; '.'; '.'; '0'; '.'; '.'; '.'; '.'; ]
    [ '.'; '.'; '.'; '.'; '0'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; ]
    [ '.'; '.'; '.'; '.'; '.'; '.'; 'A'; '.'; '.'; '.'; '.'; '.'; ]
    [ '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; ]
    [ '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; ]
    [ '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; 'A'; '.'; '.'; '.'; ]
    [ '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; 'A'; '.'; '.'; ]
    [ '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; ]
    [ '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; ]
  ]

  test
    <@ let actual = parseInput exampleInput
       actual = Ok expected @>

[<Fact>]
let ``Answer 1 for example input`` () =
  let input = parseInput exampleInput

  test
    <@ let actual = Result.bind answer1 input
       let expected: Result<_, string> = Ok 14
       actual = expected @>

[<Fact(Skip="TODO")>]
let ``Answer 2 for example input`` () =
  let input = parseInput exampleInput

  test
    <@ let actual = Result.bind answer2 input
       let expected: Result<_, string> = Ok 31
       actual = expected @>
