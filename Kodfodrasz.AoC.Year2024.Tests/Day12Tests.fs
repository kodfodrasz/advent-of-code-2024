module Kodfodrasz.AoC.Year2024.Tests.Day12Tests

open Xunit
open Swensen.Unquote.Assertions

open Kodfodrasz.AoC
open Kodfodrasz.AoC.Year2024
open Kodfodrasz.AoC.Year2024.Day12


let exampleInput1 = """
AAAA
BBCD
BBCC
EEEC
  """

[<Fact>]
let ``Parsing example input 1`` () =
  let expected = array2D [
    [ 'A'; 'A'; 'A'; 'A' ]
    [ 'B'; 'B'; 'C'; 'D' ]
    [ 'B'; 'B'; 'C'; 'C' ]
    [ 'E'; 'E'; 'E'; 'C' ]
  ]

  test
    <@ let actual = parseInput exampleInput1
       actual = Ok expected @>

[<Fact>]
let ``Answer 1 helper walk for example input 1`` () =
  let input = 
    parseInput exampleInput1
    |> Result.get

  test
    <@ let actual = walk input [] [0,0]
       let expected : region list = []
       actual = expected @>

[<Fact>]
let ``Answer 1 for example input 1`` () =
  let input = parseInput exampleInput1

  test
    <@ let actual = Result.bind answer1 input
       let expected: Result<_, string> = Ok 140
       actual = expected @>

let exampleInput2 = """
OOOOO
OXOXO
OOOOO
OXOXO
OOOOO
  """

[<Fact>]
let ``Answer 1 for example input 2`` () =
  let input = parseInput exampleInput1

  test
    <@ let actual = Result.bind answer1 input
       let expected: Result<_, string> = Ok 772
       actual = expected @>

[<Fact>]
let ``Answer 1 for example input 3`` () =
  let input = 
    Result.get <| parseInput """
RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE
"""

  test
    <@ let actual = answer1 input
       let expected: Result<_, string> = Ok 1930
       actual = expected @>


[<Fact(Skip="TODO")>]
let ``Answer 2 for example input`` () =
  let input = parseInput exampleInput1

  test
    <@ let actual = Result.bind answer2 input
       let expected: Result<_, string> = Ok 31
       actual = expected @>
