/// Represents the Day11 module for Advent of Code challenge, providing utilities
/// to parse input, count digits, and solve problems related to manipulating numbers.
module aoc24.Day11

/// Parses a string input by splitting on spaces and converting each element to an `int64`.
let parse = StringEx.splitC ' ' >> Array.map int64

/// Calculates the number of digits in a given numeric value.
///
/// This function determines the count of digits by converting the number to a floating-point representation,
/// taking the base-10 logarithm, and rounding down to the nearest integer. One is added to this value to get
/// the correct digit count.
///
/// Parameters:
///  - `num`: The number whose digits are to be counted.
///
/// Returns:
///  - An integer representing the number of digits in the given number.
///
/// Constraints:
///  - The input `num` should be a numeric value.
let inline countDigits num = 1 + (num |> float |> log10 |> int)

/// Splits a numeric value into two parts based on the specified number of digits.
/// The digits argument defines the total number of digits in the number to be split.
/// If the input number has an odd number of digits, behavior may differ based on the middle split.
/// Returns a tuple (left, right), where left refers to the higher-order digits and right
/// refers to the lower-order digits after the split.
/// Example: If the input number is 123456 and digits is 6, the result will be (123, 456).
let inline splitNum digits num =
    let factor = int digits / 2 |> pown 10L
    let left = num / factor
    let right = num - left * factor
    (left, right)

/// Applies a sequence of transformations to compute the final result based on the input list and the number of iterations (blinks).
///
/// The function processes a list of integers where:
/// - Each integer in the list undergoes a transformation based on the number of its digits and its value.
/// - If the number has 0 digits, it transforms into 1.
/// - If the number of digits is even, it splits the number into two parts (left and right) and adds both parts to the list.
/// - Otherwise, it multiplies the number by 2024 and includes this new value in the list.
///
/// The operation is repeated for each blink, and the resulting size of the list is returned as the output.
///
/// Parameters:
///   blinks: The number of iterations to apply transformations.
///   input: A string of space-separated integers to be processed.
///
/// Returns:
///   The total count of elements in the list after processing the input for the specified number of blinks.
let solve blinks input =
    let initial = input |> parse |> Array.toList

    (initial, seq { 1..blinks })
    ||> Seq.fold (fun list _ ->
        ([], list)
        ||> List.fold (fun results next ->
            let digits = countDigits next

            match next with
            | 0L -> 1L :: results
            | n when System.Int64.IsEvenInteger(digits) ->
                let left, right = splitNum digits n
                left :: right :: results
            | n -> (n * 2024L) :: results))
    |> List.length

/// Implements an optimized solution for solving the problem with a given number of blinks and input data.
/// Uses memoization to cache previously computed results for performance gains.
///
/// Parameters:
///  - `blinks`: The number of blink operations to simulate.
///  - `input`: A string representing the input data where numbers are separated by spaces.
///
/// Returns:
///  - The total sum of computed values after processing all numbers in the input with the given number of blinks.
let solveFast blinks input =
    // Dictionary of (remaining blinks * input number) -> solution
    let memorizedSolutions =
        System.Collections.Generic.Dictionary<struct (int * int64), int64>()

    let rec deep remainingBlinks num =
        let key = struct (remainingBlinks, num)

        match memorizedSolutions.TryGetValue(key) with
        | true, solution -> solution
        | false, _ ->
            let digits = countDigits num

            let deepn =
                match remainingBlinks with
                | 1 -> (fun _ -> 1L)
                | _ -> deep (remainingBlinks - 1)

            let solution =
                match num with
                | 0L -> deepn 1L
                | n when System.Int64.IsEvenInteger(digits) ->
                    let left, right = splitNum digits n
                    deepn left + deepn right
                | n -> deepn (n * 2024L)

            memorizedSolutions[key] <- solution
            solution

    input |> parse |> Array.sumBy (deep blinks)

/// Computes the result for the first part of the day 11 challenge by iteratively processing
/// input numbers based on specific transformation rules and then calculating the resulting list's length.
///
/// This function applies the following transformation for a defined number of iterations (blinks):
/// - If the number is 0, it replaces it with 1.
/// - If the number has an even number of digits, it splits the number into two parts.
/// - Otherwise, it multiplies the number by 2024.
///
/// Parameters:
/// - `blinks`: The number of iterations (transformation steps) to perform.
/// - `input`: The input string containing numbers separated by spaces.
///
/// Returns:
/// - The length of the list after applying the transformations.
let part1 = solve 25
/// Computes the solution for part 2 of the problem.
/// It utilizes the `solveFast` function with a predetermined number
/// of blinks set to 75. This implementation optimizes the computation
/// using memoization to store intermediate results and minimize redundant calculations.
let part2 = solveFast 75
/// Entry point for the Day 11 solution module. Executes the program
/// by reading inputs and applying `part1` and `part2` solvers.
let run = runReadAllText part1 part2

/// Contains tests for the logic provided in the Day11 module.
module tests =
    open Swensen.Unquote
    open Xunit

    /// Module for solving problems presented in Day 11 of Advent of Code 2024.
    let example = "125 17"

    /// Parses the input string by splitting it with a space character
    /// and converting each result to a 64-bit integer.
    [<Fact>]
    let ``Part 1 solve`` () = solve 25 example =! 55312

    /// Implements a fast solution for the problem using memoization to store and reuse results of
    /// intermediate computations, reducing redundant calculations.
    ///
    /// Optimized with a dictionary that maps a tuple containing the number of remaining operations
    /// (blinks) and a given input number to its computed solution.
    ///
    /// Parameters:
    /// - `blinks`: The number of iterations or transformations to be applied to the input.
    /// - `input`: A string of space-separated numbers to be parsed and processed.
    ///
    /// For each number in the input:
    /// - If the number is zero, it always evaluates to 1.
    /// - If the number has an even number of digits, it splits into two parts (left and right) and processes them recursively.
    /// - Otherwise, the number is multiplied by 2024 and recursively processed.
    ///
    /// Returns:
    /// The sum of the results obtained by applying the above operations to all numbers in the input.
    [<Fact>]
    let ``Part 1 solveFast`` () = solveFast 25 example =! 55312
