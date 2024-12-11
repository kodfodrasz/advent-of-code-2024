module Kodfodrasz.AoC.Year2024.Tests.Day11Tests

open Xunit
open Swensen.Unquote.Assertions

open Kodfodrasz.AoC.Year2024
open Kodfodrasz.AoC.Year2024.Day11
open System.Collections.Generic


let exampleInput = """
1 2024 1 0 9 9 2021976
  """

[<Fact>]
let ``Parsing example input`` () =
  let expected: Result<parsedinput, string> = Ok <| [
      1L; 2024L; 1L; 0L; 9L; 9L; 2021976L;
  ]

  test
    <@ let actual = parseInput exampleInput
       actual = expected @>

[<Fact>]
let ``Answer 1 for example input`` () =
  test
    <@ answer1 [125L; 17L] = Ok 55312 @>

[<Fact(Skip = "No expected value in problem")>]
let ``Answer 2 for example input`` () =
  test
    <@ answer2 [125L; 17L] = Ok 55312 @>

[<Fact>]
let ``Answer 1 helper test`` () =
  test
    <@ 
      pluto 0L = [| 1L |]
    @>
  test
    <@ 
      pluto 1L = [| 2024L |]
    @>
  test
    <@ 
      pluto 2024L = [| 20L; 24L |]
    @>
  test
    <@ 
      pluto 1000L = [| 10L; 0L |]
    @>
  test
    <@ 
      pluto 99 = [| 9L; 9L |]
    @>
  test
    <@ 
      pluto 999 = [| 2021976L |]
    @>

[<Fact>]
let ``Answer 2 helper test for example input`` () =
  test
    <@ answer2_raw 1 1 [125L; 17L] = Ok 3 @>
  test
    <@ answer2_raw 1 2 [125L; 17L] = Ok 4 @>
  test
    <@ answer2_raw 2 3 [125L; 17L] = Ok 5 @>
  test
    <@ answer2_raw 2 6 [125L; 17L] = Ok 22 @>
  test
    <@ answer2_raw 3 6 [125L; 17L] = Ok 22 @>
  test
    <@ answer2_raw 4 6 [125L; 17L] = Ok 22 @>
  test
    <@ answer2_raw 5 6 [125L; 17L] = Ok 22 @>
  test
    <@ answer2_raw 3 25 [125L; 17L] = Ok 55312 @>
  test
    <@ answer2_raw 5 25 [125L; 17L] = Ok 55312 @>
  test
    <@ answer2_raw 7 25 [125L; 17L] = Ok 55312 @>
  test
    <@ answer2_raw 10 25 [125L; 17L] = Ok 55312 @>
  test
    <@ answer2_raw 25 25 [125L; 17L] = Ok 55312 @>

[<Fact>]
let ``Answer 2 helper test for example input 2`` () =
  test
    <@ answer2_raw 1 1 [0L; ] = Ok 1 @>
  test
    <@ answer2_raw 1 1 [1L; ] = Ok 1 @>
  test
    <@ answer2_raw 1 1 [17L; ] = Ok 2 @>
  test
    <@ answer2_raw 1 1 [177L; ] = Ok 1 @>
  test
    <@ answer2_raw 1 1 [111000L; ] = Ok 2 @>
  test
    <@ answer2_raw 1 3 [11111111L; ] = Ok 8 @>
  test
    <@ answer2_raw 1 3 [10010000L; ] = Ok 4 @>
