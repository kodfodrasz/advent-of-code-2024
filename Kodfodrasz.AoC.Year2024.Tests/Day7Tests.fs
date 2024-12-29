module Kodfodrasz.AoC.Year2024.Tests.Day7Tests

open Xunit
open Swensen.Unquote.Assertions

open Kodfodrasz.AoC
open Kodfodrasz.AoC.Year2024
open Kodfodrasz.AoC.Year2024.Day7


let exampleInput = """
190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20
  """

[<Fact>]
let ``Parsing example input`` () =
  let expected = [ 
    {
      TestValue = 190L
      Terms = [10L; 19L]
    }
    {
      TestValue = 3267L
      Terms = [81L; 40L; 27L]
    }
    {
      TestValue = 83L
      Terms = [17L; 5L]
    }
    {
      TestValue = 156L
      Terms = [15L; 6L]
    }
    {
      TestValue = 7290L
      Terms = [6L; 8L; 6L; 15L]
    }
    {
      TestValue = 161011L
      Terms = [16L; 10L; 13L]
    }
    {
      TestValue = 192L
      Terms = [17L; 8L; 14L]
    }
    {
      TestValue = 21037L
      Terms = [9L; 7L; 18L; 13L]
    }
    {
      TestValue = 292L
      Terms = [11L; 6L; 16L; 20L]
    }
  ]

  test
    <@ let actual = parseInput exampleInput
       actual = Ok expected @>

[<Fact>]
let ``Answer 1 helper function: check`` () =
  let input = parseInput exampleInput |> Result.get

  test <@ true = Day7.check input[0] @>
  test <@ true = Day7.check input[1] @>
  test <@ false = Day7.check input[3] @>
  test <@ false = Day7.check input[5] @>
  test <@ true = Day7.check input[8] @>


[<Fact>]
let ``Answer 1 for example input`` () =
  let input = parseInput exampleInput

  test
    <@ let actual = Result.bind answer1 input
       let expected: Result<_, string> = Ok 3749L
       actual = expected @>

[<Fact(Skip="TODO")>]
let ``Answer 2 for example input`` () =
  let input = parseInput exampleInput

  test
    <@ let actual = Result.bind answer2 input
       let expected: Result<_, string> = Ok 31
       actual = expected @>
