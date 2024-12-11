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

[<Fact(Skip="TODO")>]
let ``Answer 2 for example input`` () =
  let input = parseInput exampleInput

  test
    <@ let actual = Result.bind answer2 input
       let expected: Result<int, string> = Ok -99999 // TODO
       actual = expected @>

[<Fact>]
let ``Answer 1 helper test`` () =
  test
    <@ 
      pluto 0L = [ 1L ]
    @>
  test
    <@ 
      pluto 1L = [ 2024L ]
    @>
  test
    <@ 
      pluto 2024L = [ 20L; 24L ]
    @>
  test
    <@ 
      pluto 1000L = [ 10L; 0L ]
    @>
  test
    <@ 
      pluto 99 = [ 9L; 9L ]
    @>
  test
    <@ 
      pluto 999 = [ 2021976L ]
    @>
