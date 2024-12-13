/// Represents the `Day03noregex` module, which contains functionality for parsing,
/// analyzing, and computing results from input strings related to multiplication
/// operations. It provides two main computations: part1 and part2.
module aoc24.Day03noregex

open System

/// Provides pattern matching for identifying single ASCII digits.
/// Matches a character and returns its corresponding integer value if it is an ASCII digit.
/// Returns `None` if the character is not a digit.
let (|Digit|_|) =
    function
    | c when Char.IsAsciiDigit c -> Some(int c - int '0')
    | _ -> None

/// Represents a module for solving Advent of Code Day 03 challenge without using regular expressions.
let (|Number|_|) delim =
    function
    | Digit a :: Digit b :: Digit c :: x :: tail when x = delim -> Some(a * 100 + b * 10 + c, tail)
    | Digit a :: Digit b :: x :: tail when x = delim -> Some(a * 10 + b, tail)
    | Digit a :: x :: tail when x = delim -> Some(a, tail)
    | _ -> None

/// Defines the module for solving Advent of Code Day 03 utilizing pattern matching for parsing operations without regular expressions.
let (|Mul|_|) =
    function
    | 'm' :: 'u' :: 'l' :: '(' :: Number ',' (a, tail) ->
        match tail with
        | Number ')' (b, tail) -> Some(a, b, tail)
        | _ -> None
    | _ -> None

/// Computes the sum of products of integer pairs in a sequence.
/// Each pair is multiplied, and the resulting products are summed together.
///
/// Returns the total sum obtained by applying multiplication to each pair and
/// accumulating the results.
let mulSums = Seq.sumBy (TupleEx.apply (*))

/// Represents a computation module for Advent of Code Day 03 problem without using regular expressions.
let part1 input =
    let rec parse output =
        function
        | Mul(a, b, tail) -> parse ((a, b) :: output) tail
        | _ :: tail -> parse output tail
        | [] -> output

    let muls = input |> List.ofSeq |> parse []
    muls |> mulSums

/// A module representing day 3's functionality without relying on regex for parsing operations.
let (|Do|_|) =
    function
    | 'd' :: 'o' :: '(' :: ')' :: tail -> Some tail
    | _ -> None

/// Provides functionality to process input strings without regular expressions.
let (|Dont|_|) =
    function
    | 'd' :: 'o' :: 'n' :: ''' :: 't' :: '(' :: ')' :: tail -> Some tail
    | _ -> None

/// Represents the state of operations that can either be enabled or disabled.
type DoState =
    /// Represents the states that control the parsing behavior in the context of the `part2` function logic.
    ///
    /// - `Enabled`: Indicates that parsing is in an active state; operations will be processed.
    /// - `Disabled`: Indicates that parsing is in an inactive state; operations will not be processed.
    | Enabled
    /// Represents different matches for digit extraction from a char.
    | Disabled

/// Processes input strings containing specific patterns and operations such as "do", "don't", and "mul",
/// and calculates the sum of the multiplication of number pairs.
///
/// Parses the input to toggle active states ("do" enables, "don't" disables) and only processes
/// multiplication operations when in an enabled state.
///
/// Filters input sequences to extract valid multiplication pairs when the enabled state is "on",
/// then computes their product sums.
///
/// Arguments:
/// - input: A sequence of characters comprising patterns, numbers, and valid operations.
///
/// Returns:
/// - An integer representing the sum of the results of valid multiplications processed when enabled.
let part2 input =
    let rec parse output doState =
        function
        | Do tail -> parse output Enabled tail
        | Dont tail -> parse output Disabled tail
        | Mul(a, b, tail) when doState.IsEnabled -> parse ((a, b) :: output) doState tail
        | _ :: tail -> parse output doState tail
        | [] -> output

    let muls = input |> List.ofSeq |> parse [] Enabled
    muls |> mulSums

/// Unit tests for solutions and functionality defined in Day03_noregex.fs module.
/// Provides test cases for parsing and processing functionality in various scenarios,
/// ensuring correctness and expected behavior. Includes both example-based and
/// real-world data tests for `part1` and `part2` functions.
module tests =
    open Swensen.Unquote
    open Xunit

    /// Example input string used in tests for the implemented functionality.
    let example1 =
        "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

    /// Contains solutions for Advent of Code Day 03 puzzle using a non-regex-based approach.
    [<Fact>]
    let ``Part 1 example`` () = part1 example1 =! 161

    /// Represents a module for solutions to Advent of Code Day 03 challenges without relying on regular expressions.
    [<Theory>]
    [<InlineData("xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")>]
    [<InlineData("xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()\n?mul(8,5))")>]
    let ``Part 2 example`` (input) = part2 input =! 48

    /// Contains functions for parsing and evaluating specific patterns in input strings related to the advent of code problem.
    [<Fact>]
    let ``Part 1 realworld`` () =
        System.IO.File.ReadAllText("../../../inputs/day03.txt") |> part1 =! 156388521


    /// Provides patterns and operations for parsing input and performing computations in Advent of Code Day 3, without using regular expressions.
    [<Fact>]
    let ``Part 2 realworld`` () =
        System.IO.File.ReadAllText("../../../inputs/day03.txt") |> part2 =! 75920122
