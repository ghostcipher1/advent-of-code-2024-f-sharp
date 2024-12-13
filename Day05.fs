/// Represents module Day05 with primary functionalities for solving specific
/// Advent of Code challenges related to processing and analyzing rules
/// and pages data.
module aoc24.Day05

/// Module defining the functionality related to operations involving pages.
module Pages =
    /// Computes the middle element of a given list of integers.
    /// Throws an exception if the list is empty.
    /// The function assumes zero-based indexing and returns the element
    /// at the middle index, calculated as `Length / 2`.
    let middle (p: int list) = p[p.Length / 2]

/// Parses the input data to extract rules and pages information.
/// The input is an array of strings, where the rules and pages are separated by an empty string.
/// Rules are split by the '|' character, and pages are split by the ',' character.
/// Returns a tuple containing a list of parsed rules and a list of parsed pages.
let parse input =
    let parseRules =
        Seq.map (StringEx.splitC '|' >> (fun p -> (int p.[0], int p.[1]))) >> Seq.toList

    let parsePages =
        Seq.map (StringEx.splitC ',' >> Seq.map int >> Seq.toList) >> Seq.toList

    let splitAt = input |> Array.findIndex ((=) "")
    (parseRules input.[0 .. splitAt - 1], parsePages input.[splitAt + 1 ..])

/// Contains the modules and functions for solutions to day 5 challenges.
let part1 input =

    let rules, pages = parse input

    let ruleMap = rules |> List.groupBy snd |> Map

    let findBrokenRules pages =
        let rec loop pNums brokenRules =
            match pNums with
            | cur :: tail ->
                let matchingRules = ruleMap |> Map.tryFind cur |> Option.defaultValue []

                let brokenMatchingRules =
                    matchingRules |> List.filter (fun rule -> tail |> List.contains (rule |> fst))

                loop tail (brokenMatchingRules @ brokenRules)

            | [] -> brokenRules

        loop pages []

    pages
    |> List.filter (fun p -> (findBrokenRules p |> List.length) = 0)
    |> List.sumBy Pages.middle


/// Computes the result for the second part of the problem based on the provided input.
///
/// This function performs the following steps:
/// - Parses the input into rules and pages using `parse`.
/// - Converts the parsed rules into a set for efficient lookup.
/// - Defines a comparer function to sort pages based on the rules.
/// - Sorts each page's list according to the custom comparer.
/// - Filters out pages where the original order of elements does not match the sorted order
///   and retains only the corrected ones.
/// - Sums specific values from the corrected pages using the `Pages.middle` function.
///
/// Parameter `input`: A sequence of strings representing the input data.
/// Returns: An integer representing the summed value based on corrected pages.
let part2 input =
    let rules, pagesList = parse input

    let rulesSet = rules |> Set

    let comparer a b =
        if rulesSet |> Set.contains (a, b) then -1
        elif rulesSet |> Set.contains (b, a) then 1
        else 0

    let sortedPagesList = pagesList |> List.map (List.sortWith comparer)

    let correctedPagesList =
        List.zip pagesList sortedPagesList
        |> List.choose (function
            | original, sorted when original <> sorted -> Some sorted
            | _ -> None)

    correctedPagesList |> List.sumBy Pages.middle


/// Executes the main functionality for Day 05 by reading input, processing it,
/// and applying both part1 and part2 solutions to the input data.
///
/// This function serves as the entry point for Day 05's solutions.
/// It retrieves all input lines from the provided file and applies
/// the logic for each part of the problem using `part1` and `part2`.
///
/// Parameters:
///   part1 - A function representing the solution logic for part 1.
///   part2 - A function representing the solution logic for part 2.
///
let run = runReadAllLines part1 part2

/// Contains test cases for the Day 05 problem module, providing validation of parsing and computation functions.
module tests =
    open Swensen.Unquote
    open Xunit

    /// Contains functionality for solving Day 05 of Advent of Code challenges.
    let example =
        [| "47|53"
           "97|13"
           "97|61"
           "97|47"
           "75|29"
           "61|13"
           "75|53"
           "29|13"
           "97|29"
           "53|29"
           "61|53"
           "97|53"
           "61|29"
           "47|13"
           "75|47"
           "97|75"
           "47|61"
           "75|61"
           "47|29"
           "75|13"
           "53|13"
           ""
           "75,47,61,53,29"
           "97,61,53,29,13"
           "75,29,13"
           "75,97,47,61,53"
           "61,13,29"
           "97,13,75,29,47" |]

    /// Represents the module for Advent of Code Day 05 solutions.
    [<Fact>]
    let ``Parse example`` () =
        let rules, pages = parse example
        Seq.head rules =! (47, 53)
        Seq.last rules =! (53, 13)
        Seq.head pages =! [ 75; 47; 61; 53; 29 ]
        Seq.last pages =! [ 97; 13; 75; 29; 47 ]

    /// Module contains logic related to the solution of Advent of Code Day 5 challenge.
    [<Fact>]
    let ``Part 1 example`` () = part1 example =! 143

    /// Contains the implementation for solving Day 05's challenge in Advent of Code.
    [<Fact>]
    let ``Part 2 example`` () = part2 example =! 123
