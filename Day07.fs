/// Day07 module containing functions and logic for solving the specific Advent of Code challenges.
module aoc24.Day07

/// Parses a single input line into a tuple comprising a head value and a list of tail values.
/// The input string is expected to follow a specific format where elements are separated by spaces or ": ".
/// The head value is the first number in the parsed result, while the remaining numbers constitute the tail list.
/// Throws an exception if the input string does not conform to the expected format.
///
/// input: The input string to be parsed.
///
/// Returns: A tuple where the first element is the head value (of type `int64`)
/// and the second element is a list of tail values (of type `int64 list`).
let parseLine input =
    match input |> StringEx.splitSs [| " "; ": " |] |> Seq.map int64 |> Seq.toList with
    | head :: tail -> (head, tail)
    | _ -> failwith $"parse error: {input}"

/// Parses an array of strings using the `parseLine` function. Each line is processed and converted into a tuple of an integer and a list of integers.
///
/// The function operates on an array of strings, where each string is expected to contain a colon-separated format with a target integer and a series of integers.
///
/// This function is used as part of the problem-solving process to transform input data into a usable structured format.
let parse = Array.map parseLine

/// Generates all possible combinations of a given array of elements by picking a specific number of items (times) from the array.
///
/// Parameters:
///   times: The number of elements to pick for each combination.
///   array: An array of elements to generate combinations from.
///
/// Returns:
///   A sequence containing all combinations as lists where the order of elements in the combination matches the order of picking.
///
/// If times is 1, it directly yields arrays of single elements. Otherwise, the function recursively generates combinations by
/// building combinations of size times - 1 and adding one more element to each from the array.
let allCombinations times (array: 'a array) =
    let rec loop times list =
        seq {
            for item in array do
                match times with
                | 1 -> yield item :: list
                | _ -> yield! loop (times - 1) (item :: list)
        }

    loop times []

/// Finds all possible solutions to a given computational problem based on a set of operations and an input equation.
/// The function generates all combinations of operations (as specified in `ops`) applied to the list of numbers
/// and checks if the result matches the expected output.
///
/// Parameters:
///   ops: An array of binary arithmetic operation functions (e.g., (+), (*), etc.) to be applied between numbers.
///   (expected, numbers): A tuple where `expected` is the target result, and `numbers` is a list of integers to
///                        apply the operations to.
///
/// Returns:
///   A sequence of operation combinations (as lists) that produce the `expected` result when applied to the `numbers`.
let findSolutions ops (expected, numbers) =
    ops
    |> allCombinations (List.length numbers - 1)
    |> Seq.filter (fun ops ->
        let result =
            (numbers.Head, numbers.Tail, ops)
            |||> List.fold2 (fun acc number op -> op acc number)

        result = expected)

/// Solves a problem given a set of operations and input data.
///
/// Performs the following steps:
/// 1. Parses the input to extract expected results and numbers.
/// 2. Filters the parsed input to find only valid cases where operations can result in the expected value.
/// 3. Sums the first elements (expected results) of the filtered cases.
///
/// Parameters:
/// - `ops`: An array of binary operations to be applied for solving the equations.
/// - `input`: An array of strings representing the equations to solve, where each equation includes an expected result and a list of numbers.
///
/// Returns:
/// The sum of the valid first elements (expected results) for the cases where solutions are found.
let solve ops input =
    input
    |> parse
    |> Array.Parallel.filter (fun e -> (findSolutions ops e) |> Seq.exists (fun _ -> true))
    |> Array.sumBy fst

/// Calculates the result for Part 1 of the problem using predefined operations.
/// Applies the solution function with addition and multiplication operators to the input data.
let part1 = solve [| (+); (*) |]

/// Provides functionality for parsing a specific input format.
let inline (.||) a b =
    let bDigitCount = (log10 (float b) |> int) + 1
    (pown 10L bDigitCount) * a + b

/// Calculates the sum of the first numbers in input that have solutions matching the operations set, including addition, multiplication, and a custom concatenation operation.
let part2 = solve [| (+); (*); (.||) |]

/// Executes the main functionality by reading input data, solving both parts of the problem, and outputting the results.
/// This function combines the solutions of part1 and part2 by utilizing the common input processing pipeline.
/// Assumes that `runReadAllLines` takes care of handling the input and output operations for the problem.
let run = runReadAllLines part1 part2

/// Contains unit tests for the Day07 module, validating the functionality of parsing, solving, and handling input data.
module tests =
    open Swensen.Unquote
    open Xunit

    ///
    /// Contains solutions and utilities for solving the Day 07 challenge.
    let example =
        [| "190: 10 19"
           "3267: 81 40 27"
           "83: 17 5"
           "156: 15 6"
           "7290: 6 8 6 15"
           "161011: 16 10 13"
           "192: 17 8 14"
           "21037: 9 7 18 13"
           "292: 11 6 16 20" |]

    /// The module containing logic for day 7 of the advent of code challenge, providing parsing
    /// and computation functions to solve the puzzle.
    [<Fact>]
    let ``Part 1 example`` () = part1 example =! 3749

    /// Tests the functionality of `part2` using an example dataset.
    /// Validates that the result produced by `part2` matches the expected value for the example provided.
    [<Fact>]
    let ``Part 2 example`` () = part2 example =! 11387

    /// Finds the number of valid solutions for a given equation using the provided operations.
    /// This function utilizes all possible combinations of the operations to evaluate the equation
    /// and returns the count of solutions that satisfy the expected result.
    /// - `ops`: An array of functions representing the operations to be applied in the equation.
    /// - `equation`: A tuple where the first element is the expected result and the second element is a list of numbers.
    /// Returns the total count of valid solutions.
    let findSolutionCount ops equation =
        findSolutions ops equation |> Seq.length

    /// Contains example data for testing and validating the solution for part 1.
    ///
    /// Includes inputs as strings paired with their respective expected solution counts.
    let data1 =
        [ "190: 10 19", 1
          "3267: 81 40 27", 2
          "292: 11 6 16 20", 1
          "21037: 9 7 18 13", 0 ]

    /// Parses a single line of input into a tuple containing an integer followed by a list of integers.
    /// Throws an exception if the input format is invalid.
    /// - input: The string input to parse.
    [<Theory; MemberData(nameof data1)>]
    let ``Part 1 solution count`` line count =
        line |> parseLine |> findSolutionCount [| (+); (*) |] =! count


    ///
    /// Data for the second set of tests, represented as a list of tuples where each tuple contains
    /// a string representation of the input and an expected solution count.
    let data2 =
        [ "156: 15 6", 1
          "7290: 6 8 6 15", 1
          "192: 17 8 14", 1
          "21037: 9 7 18 13", 0 ]

    /// Part 2 solution count
    /// Test method for verifying the solution count for part 2 of the problem.
    /// Utilizes additional operator (.||) for concatenation in testing operations.
    ///
    /// Parameters:
    ///  - `line`: A single equation string that includes the expected result and a series of numbers.
    ///  - `count`: The expected count of valid solutions for the given equation and operations.
    ///
    /// Asserts:
    ///  - Confirms that the number of valid solutions matches the expected count, using the given equation and operators.
    [<Theory; MemberData(nameof data2)>]
    let ``Part 2 solution count`` line count =
        line |> parseLine |> findSolutionCount [| (+); (*); (.||) |] =! count
