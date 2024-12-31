module Kodfodrasz.AoC.Year2024.Tests.Day10Tests

open Xunit
open Swensen.Unquote.Assertions

open Kodfodrasz.AoC
open Kodfodrasz.AoC.Year2024
open Kodfodrasz.AoC.Year2024.Day10


let exampleInput = """
89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732
  """

[<Fact>]
let ``Parsing example input`` () =
  let expected = [|
    [| 8; 9; 0; 1; 0; 1; 2; 3; |]
    [| 7; 8; 1; 2; 1; 8; 7; 4; |]
    [| 8; 7; 4; 3; 0; 9; 6; 5; |]
    [| 9; 6; 5; 4; 9; 8; 7; 4; |]
    [| 4; 5; 6; 7; 8; 9; 0; 3; |]
    [| 3; 2; 0; 1; 9; 0; 1; 2; |]
    [| 0; 1; 3; 2; 9; 8; 0; 1; |]
    [| 1; 0; 4; 5; 6; 7; 3; 2; |]
  |]

  test
    <@ let actual = parseInput exampleInput
       actual = Ok expected @>

[<Fact>]
let ``Answer 1 for example input`` () =
  let input = parseInput exampleInput

  test
    <@ let actual = Result.bind answer1 input
       let expected: Result<_, string> = Ok 36
       actual = expected @>

[<Fact>]
let ``Answer 1 for example input: single 2`` () =
  let input = parseInput """
...0...
...1...
...2...
6543456
7.....7
8.....8
9.....9
  """

  test
    <@ let actual = Result.bind answer1 input
       let expected: Result<_, string> = Ok 2
       actual = expected @>

[<Fact>]
let ``Answer 1 for example input: single 4`` () =
  let input = parseInput """
..90..9
...1.98
...2..7
6543456
765.987
876....
987....
  """

  test
    <@ let actual = Result.bind answer1 input
       let expected: Result<_, string> = Ok 4
       actual = expected @>

[<Fact(Skip="TODO")>]
let ``Answer 2 for example input`` () =
  let input = parseInput exampleInput

  test
    <@ let actual = Result.bind answer2 input
       let expected: Result<_, string> = Ok 31
       actual = expected @>
