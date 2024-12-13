/// Provides functionality and calculations for Day 01 problem solutions.
module aoc24.Day01

/// Parses an array of strings into a tuple of two integer arrays.
/// For each string in the input array, splits the string into two parts
/// using a triple-space delimiter and converts the parts into integers.
/// The resulting integers are separated into two arrays.
///
/// Returns:
/// A tuple containing two integer arrays where the first array contains the
/// first integers from the split parts and the second array contains the
/// second integers from the split parts.
let parse =
    Array.map (fun l ->
        let parts = l |> StringEx.splitS "   "
        (int parts[0], int parts[1]))
    >> Array.unzip

/// Computes the result for part 1 of Day 01 problem by performing several operations
/// such as parsing, sorting, and calculating differences for all pairs in the input.
/// The function utilizes helper utilities like `StringEx.splitS`, `TupleEx.map`,
/// and `TupleEx.apply` for processing the data.
/// - input: An array of strings, where each string is expected to contain two integers
///   separated by three spaces, representing pairs of numbers.
/// - Returns: An integer that represents the computed result based on the problem's
///   specific requirements (e.g., sum of absolute differences after sorting).
let part1 input =
    let sortedPairs =
        input |> parse |> TupleEx.map Array.sort |> TupleEx.apply Array.zip

    (0, sortedPairs)
    ||> Array.fold (fun acc pair -> acc + (pair |> TupleEx.apply (-) |> abs))

/// Computes the result for part 2 of the problem.
/// Calculates the sum based on the parsed input, where for each element in `left`,
/// the count of matching elements in `right` is looked up from a precomputed counts map.
/// The multiplication of the `left` value and its corresponding count in the map contributes to the accumulated result.
///
/// Parameters:
///   input: The input array of strings to process.
///
/// Returns:
///   An integer representing the computed result for part 2 of the problem.
let part2 input =
    let left, right = parse input

    let counts =
        right |> Array.groupBy id |> Array.map (TupleEx.mapSnd Array.length) |> Map

    (0, left)
    ||> Array.fold (fun acc left ->
        let rightCounts = counts |> Map.tryFind left |> Option.defaultValue 0
        acc + left * rightCounts)

/// Executes the solution for Day 1 of Advent of Code 2024.
/// Combines `part1` and `part2` functionality to solve both parts of the problem.
/// Utilizes `runReadAllLines` to read input lines from a file, parse the input,
/// and compute results using `part1` and `part2` functions.
/// The results for both parts are formatted and displayed as output.
let run = runReadAllLines part1 part2

/// Contains unit tests for the functionality provided in the Day01 module.
module tests =
    open Swensen.Unquote
    open Xunit

    /// Provides functionality for Advent of Code Day 01 computations.
    let example =
        [| "3   4" //
           "4   3"
           "2   5"
           "1   3"
           "3   9"
           "3   3" |]

    /// Represents the Advent of Code Day 01 challenge module.
    [<Fact>]
    let ``Part 1 example`` () = part1 example =! 11

    /// Part 2 example test case for Advent of Code, verifying the behavior of the `part2` function.
    ///
    /// This function executes a unit test using the `example` input data and compares the output of `part2`
    /// to the expected result, ensuring its accuracy and correctness.
    ///
    /// The `example` input represents a series of string lines denoting pairs of integers. `part2` processes
    /// this input to compute a specific calculation, and this test ensures the implementation aligns with
    /// the expected behavior.
    ///
    /// Validation is performed using the `=!` operator provided by the `Swensen.Unquote` library, which
    /// creates a clear, readable assertion.
    ///
    /// Expected result: Verifies if the output of `part2 example` equals 31.
    [<Fact>]
    let ``Part 2 example`` () = part2 example =! 31
