/// Represents the module responsible for solving Day 03 of the Advent of Code challenges
module aoc24.Day03

open FSharp.Text.RegexProvider
open FSharp.Text.RegexExtensions

/// Represents a strongly typed regular expression for matching the pattern
/// `mul(factor1, factor2)` where `factor1` and `factor2` are numeric values.
/// This type uses FSharp.Text.RegexProvider to provide a strongly typed regular
/// expression interface.
type MulRegex = Regex< @"mul\((?<factor1>\d+),(?<factor2>\d+)\)" >

/// Parses all 'mul(factor1, factor2)' occurrences in the given input string.
/// Returns a sequence of tuples where each tuple contains the parsed integer values
/// of `factor1` and `factor2`.
let parseMuls input =
    MulRegex().TypedMatches(input)
    |> Seq.map (fun m -> (m.factor1.AsInt, m.factor2.AsInt))

/// A strongly-typed regular expression matcher for the pattern
/// `(?s)(?:do\(\)|^)(?<enabledOps>.*?)(?:don't\(\)|$)`.
/// Captures the portion of a string identified by the named group `enabledOps`.
type DoRegex = Regex< @"(?s)(?:do\(\)|^)(?<enabledOps>.*?)(?:don't\(\)|$)" >

/// Parses the input string to extract and evaluate "enabled" multiplication operations.
/// A "enabled" operation is recognized by the `do`/`don't` delimiters, where only the content
/// within the `do` context is parsed for valid multiplication expressions of the form `mul(x,y)`.
/// The parsed values are returned as a sequence of tuples of integers representing the multiplication operands.
let parseEnabledMuls input =
    DoRegex().TypedMatches(input)
    |> Seq.map _.enabledOps.Value
    |> Seq.collect parseMuls

/// Computes the sum of the results of applying a multiplication operation
/// to each tuple of integers in a sequence.
///
/// Each tuple contains two integers, which are multiplied together, and the resulting values
/// are summed to produce the final result.
///
/// The function expects a sequence of pairs (tuples) of integers as input.
let mulSums = Seq.sumBy (TupleEx.apply (*))

/// Computes the sum of all parsed multiplication operations from the input string.
/// The input string is expected to contain 'mul(x,y)' patterns, where x and y are integers.
/// This function parses all valid multiplication expressions and computes their product,
/// summing up the results.
///
/// Parameters:
///   input: The input string containing 'mul(x,y)' patterns.
///
/// Returns:
///   The sum of all evaluated multiplication expressions.
let part1 input = input |> parseMuls |> mulSums
/// Computes the sum of all multiplication pairs specified within enabled operations in the input string.
/// Enabled operations are extracted from segments surrounded by `do()` and `don't()`.
/// Extracted multiplications are then parsed and their results are summed.
/// Uses `parseEnabledMuls` to extract the relevant multiplication pairs from the input string and `mulSums` to calculate the sum.
/// Input: A string containing potential operations and multiplication pairs.
/// Output: The total sum of all multiplications within the enabled operations.
let part2 input = input |> parseEnabledMuls |> mulSums
/// Executes the main logic by reading input data, processing it using part1 and part2 functions,
/// and formatting the results for output. The input is read as a single string, and results
/// include execution times for both parts.
let run = runReadAllText part1 part2

/// Contains unit tests for the Day03 module.
module tests =
    open Swensen.Unquote
    open Xunit

    /// Represents a constant example1 input string that demonstrates various cases, including valid and invalid patterns
    /// for parsing multiplication operations and enabling operations.
    let example1 =
        "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

    /// A strongly typed regex for matching multiplication operations with two factors.
    /// Captures groups "factor1" and "factor2" which represent the two operands to multiply.
    [<Fact>]
    let ``Part 1 example`` () = part1 example1 =! 161

    /// Represents the main module for solving Day 3 of the Advent of Code 2024.
    [<Theory>]
    [<InlineData("xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")>]
    [<InlineData("xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()\n?mul(8,5))")>]
    let ``Part 2 example`` (input) = part2 input =! 48
