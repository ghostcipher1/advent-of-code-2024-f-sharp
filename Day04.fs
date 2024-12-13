/// Contains solutions for Day 04 of the Advent of Code challenge.
module aoc24.Day04

/// Represents a module for working with points in a grid or 2D coordinate system.
module Point =
    /// Operations and utilities for Advent of Code Day 04 challenge.
    let op f (a, b) (c, d) = (f a c, f b d)
    /// Adds two 2D points represented as tuples (x, y) by applying element-wise addition.
    ///
    /// Parameters:
    ///   a: A tuple (x1, y1) representing the first 2D point.
    ///   b: A tuple (x2, y2) representing the second 2D point.
    ///
    /// Returns:
    ///   A tuple (x, y) representing the result of adding the two points element-wise.
    let add = op (+)

    /// Represents a list of all possible directional movements for a 2D grid.
    /// The directions include all combinations of the values -1, 0, and 1,
    /// except for the tuple (0, 0), which represents no movement.
    /// Used for navigating or determining neighbors in a grid-like structure.
    let allDirections =
        let directions = [ -1; 0; 1 ]
        List.allPairs directions directions |> List.except [ (0, 0) ]

/// Provides functions to work with two-dimensional grids represented as arrays of strings.
module Grid =
    /// Represents the main module for the Advent of Code 2024 Day 04 task.
    let at (x, y) (grid: string array) = grid[y][x]

    /// Tries to retrieve an element at the specified 2D coordinate (x, y) within a string array grid.
    /// If the specified coordinates are out of bounds, returns None.
    /// Otherwise, returns Some containing the character at the given coordinates.
    let tryAt (x, y) (grid: string array) =
        if y >= 0 && y < grid.Length && x >= 0 && x < grid[y].Length then
            Some(grid[y][x])
        else
            None

    /// Iterates through all positions of a grid represented as a sequence of strings, returning a sequence of all (x, y) coordinate pairs.
    /// Each pair (x, y) corresponds to the indices of a character within the 2D grid, where `x` represents the column and `y` represents the row.
    /// Example output for a grid of height `h` and width `w` would yield all possible pairs within the bounds (0 <= x < w, 0 <= y < h).
    let allPos (grid: string array) =
        seq {
            for y = 0 to grid.Length - 1 do
                for x = 0 to grid[y].Length - 1 do
                    yield (x, y)
        }

    /// Finds all positions of a specified character in a given 2D grid.
    ///
    /// The grid is represented as an array of strings, where each string is a row in the grid.
    /// The character's positions are returned as a list of tuples, where each tuple represents
    /// an `(x, y)` coordinate in the grid.
    ///
    /// Parameters:
    ///   chr: The character to search for in the grid.
    ///   grid: The grid represented as an array of strings.
    ///
    /// Returns:
    /// A list of `(x, y)` tuples representing the positions of the specified character in the grid.
    let findChars chr grid =
        grid |> allPos |> Seq.filter (fun p -> grid |> at p = chr) |> List.ofSeq

///
/// This module defines the solution for Day 04. It contains utility functions for working with
/// two-dimensional points and grids, as well as the main implementations for the problem's parts.
let part1 input =

    let startPoints = input |> Grid.findChars 'X'

    let rec findString start direction search =
        let next = Point.add start direction

        match search with
        | chr :: tail when input |> Grid.tryAt next |> Option.contains chr -> findString next direction tail
        | [] -> Some next
        | _ -> None

    let findFromStart start search =
        Point.allDirections
        |> List.choose (fun dir -> findString start dir (search |> List.ofSeq))

    let allEnds = startPoints |> List.collect (fun p -> findFromStart p "MAS")

    allEnds.Length

/// Implements the solution for the second part of the Day04 problem.
///
/// Analyzes the input grid to determine the number of valid points where specific
/// diagonal patterns starting from 'A' are found. Specifically:
/// - Checks for 'M' and 'S' in both diagonals originating from a given point.
/// - A valid point is counted only if both diagonals match the patterns simultaneously.
///
/// Parameters:
///     input: A grid of strings (string array) representing the puzzle input.
///
/// Returns:
///     The total count of valid points in the input grid.
let part2 input =

    let startPoints = input |> Grid.findChars 'A'

    let checkDiagonal start =
        let diagonals = [ [ (1, 1); (-1, -1) ]; [ (1, -1); (-1, 1) ] ]

        let matchingDiagonals =
            diagonals
            |> List.choose (fun diagonal ->
                let diagonalChars =
                    diagonal |> List.choose (fun p -> input |> Grid.tryAt (Point.add start p))

                match diagonalChars with
                | [ 'M'; 'S' ]
                | [ 'S'; 'M' ] -> Some diagonalChars
                | _ -> None)

        matchingDiagonals.Length = 2

    startPoints |> List.filter checkDiagonal |> List.length


/// Executes the main function for the Advent of Code Day 04 challenge.
/// It reads input from a file using `runReadAllLines`, and computes results for both part 1 and part 2 of the puzzle.
/// The results are formatted and returned as a string.
///
/// Returns: Formatted string containing the results of part 1 and part 2 executions.
let run = runReadAllLines part1 part2

/// Provides unit test cases for verifying the functionality of the solution for Day04.
module tests =
    open Swensen.Unquote
    open Xunit

    /// Provides the solution for Day 4 of Advent of Code 2024.
    let example =
        [| "MMMSXXMASM"
           "MSAMXMSMSA"
           "AMXSXMAAMM"
           "MSAMASMSMX"
           "XMASAMXAMM"
           "XXAMMXXAMA"
           "SMSMSASXSS"
           "SAXAMASAAA"
           "MAMMMXMMMM"
           "MXMXAXMASX" |]

    /// Contains the main logic and utilities for the Advent of Code Day 4 solutions.
    [<Fact>]
    let ``Part 1 example`` () = part1 example =! 18

    /// Part 2 example test case for validating the solution of part2 function.
    ///
    /// Verifies the correctness of the `part2` function against the provided
    /// `example` input.
    [<Fact>]
    let ``Part 2 example`` () = part2 example =! 9
