/// Contains implementations of the solutions for Day 02's Advent of Code problem.
module aoc24.Day02

/// Represents a module for solving the challenges in Day 02 of Advent of Code 2024.
module Report =
    /// Parses a string consisting of integers separated by a specific delimiter into a list of integers.
    /// The delimiter used for splitting is the space character (' ').
    let parse = StringEx.splitC ' ' >> Array.map int >> List.ofArray

    /// Determines if a given report (a list of integers) is "safe" according to specific criteria.
    ///
    /// The function calculates the differences between each pair of consecutive elements
    /// in the report and evaluates if all these differences lie within the range [1, 3] or [-3, -1].
    ///
    /// Parameters:
    ///  - `report`: A list of integers representing the report to be analyzed.
    ///
    /// Returns:
    ///  - A boolean value indicating whether the report is safe.
    let isSafe report =
        let changes = report |> List.pairwise |> List.map (TupleEx.apply (-))

        let test lower upper =
            changes |> List.forall (fun c -> c >= lower && c <= upper)

        test 1 3 || test -3 -1

    /// Checks if any permutation of the given report is safe when one element is removed.
    /// A report is determined to be safe if differences between consecutive elements in
    /// the modified report fall within a specified range. The operation generates all
    /// permutations by removing a single element from the report, and checks if at least
    /// one of these is safe.
    ///
    /// Parameters:
    ///   report: A list of integers representing the input report.
    ///
    /// Returns:
    ///   A boolean indicating whether any of the permutations of the report is considered safe.
    let isSafeWithDamper report =
        let permutations =
            [ 0 .. (report |> List.length) - 1 ]
            |> List.map (fun i -> report |> List.removeAt i)

        permutations |> List.exists isSafe

/// Applies a given function to filter and process input data. It parses lines of
/// strings into integer lists, filters them using the provided chooser function,
/// and returns the count of elements that satisfy the condition.
///
/// chooser : ('a -> bool) -> A function to determine if a processed report is valid.
/// 'a -> int : Returns the number of reports that pass the specified conditions.
let solve chooser =
    Array.map Report.parse >> Seq.filter chooser >> Seq.length

/// Solves the first part of the problem by filtering parsed reports using the `Report.isSafe` function.
/// Utilizes the `solve` function to process input data and count the valid entries.
let part1 = solve Report.isSafe

/// Solves part 2 of the given problem by applying the `isSafeWithDamper` function on the parsed report data.
/// This function filters the data using the condition specified in `Report.isSafeWithDamper` and calculates the count of valid entries.
let part2 = solve Report.isSafeWithDamper

/// Executes the solution for both parts of the Day02 problem by reading input,
/// applying the respective solution logic, and formatting the results.
let run = runReadAllLines part1 part2

/// Test module for validating the Day02 solution.
module tests =
    open Swensen.Unquote
    open Xunit

    /// Provides reporting-related functionalities for analyzing and validating reports.
    let example =
        [| "7 6 4 2 1" //
           "1 2 7 8 9"
           "9 7 6 2 1"
           "1 3 2 4 5"
           "8 6 4 4 1"
           "1 3 6 7 9" |]

    /// Provides functions to parse, validate, and process input reports
    /// for Advent of Code Day 2 solution.
    [<Fact>]
    let ``Part 1 example`` () = part1 example =! 2

    /// Tests for the part2 function of the Advent of Code 2024 Day 02 solution.
    [<Fact>]
    let ``Part 2 example`` () = part2 example =! 4
