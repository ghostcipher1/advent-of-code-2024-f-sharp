/// Represents functionalities for the Advent of Code 2024, Day 10 challenges.
module aoc24.Day10

/// Represents a 2D point using a structure of two integers.
type Point = (struct (int * int))

/// Represents a two-dimensional point with integer coordinates.
/// This is a value type (struct tuple) designed for lightweight use in grid-based operations.
module Point =
    /// Adds two points represented as a structured tuple (Point).
    /// The function takes two points, each being a structured tuple of two integers,
    /// and returns their sum as a new point.
    let inline add ((a, b): Point) ((c, d): Point) = struct (a + c, b + d)

/// The Grid module provides utility functions for operations
/// to interact and manipulate 2D grids represented as string arrays.
module Grid =
    ///)
    let inline at (struct (x, y)) (grid: string array) =
        if y >= 0 && y < grid.Length && x >= 0 && x < grid[y].Length then
            grid[y][x]
        else
            ' '

    /// Generates a sequence of all possible positions (coordinates) in the given grid.
    /// Each position is represented as a Point struct (x, y), where x is the horizontal coordinate
    /// and y is the vertical coordinate of a grid cell.
    ///
    /// Parameters:
    ///   grid: A 2D array of strings where each string represents a row of the grid.
    ///
    /// Returns:
    ///   A sequence of Point structs, representing all valid (x, y) positions within the grid.
    let allPos (grid: string array) =
        seq {
            for y = 0 to grid.Length - 1 do
                for x = 0 to grid[y].Length - 1 do
                    Point(x, y)
        }

    /// Filters the positions of all occurrences of the specified character in the given grid.
    /// Returns a list of Point structs representing the coordinates where the character is found.
    ///
    /// Parameters:
    ///   chr: The character to search for in the grid.
    ///   grid: The grid, represented as a string array, where the search is performed.
    ///
    /// Returns:
    ///   A list of Point structs indicating every position in the grid that contains the specified character.
    let findChars chr grid =
        grid |> allPos |> Seq.filter (fun p -> grid |> at p = chr) |> List.ofSeq


/// Solves the puzzle using the provided trail tail scorer function and input grid.
///
/// This function identifies all trail heads in the input grid marked by the character '0'.
/// It calculates the score for each trail by tracing its path recursively through increasing
/// heights until the highest point '9' is reached. The resulting scores for all trails are then combined
/// using the provided trailTailScorer function to calculate the final result.
///
/// Parameters:
///   trailTailScorer: A function determining how to compute the final score from the discovered trail tails.
///   input: A string array representing the grid in which the trails are searched.
///
/// Returns:
///   An integer value representing the total score of all trails in the grid, computed using the trailTailScorer function.
let solve trailTailScorer input =
    let trailHeads = input |> Grid.findChars '0'
    let directions = [ Point(0, -1); Point(-1, 0); Point(0, 1); Point(1, 0) ]

    let scoreTrail head =
        let rec findTrailTails curHeight pos =
            match curHeight with
            | '9' -> [ pos ]
            | _ ->
                directions
                |> List.map (Point.add pos)
                |> List.filter (fun p -> (input |> Grid.at p) = curHeight + char 1)
                |> List.collect (findTrailTails (curHeight + char 1))

        findTrailTails '0' head |> trailTailScorer

    trailHeads |> List.sumBy scoreTrail


///
/// Represents a solution to Day 10, Part 1 of the Advent of Code tasks.
/// This function uses the `solve` method with a trail scoring function that counts distinct tail positions of a trail.
/// The grid input is used to determine the positions and lengths of numeric trails defined by '0'-'9'.
/// Computes the total score of all trails starting from their identified head positions.
let part1 = solve (Seq.distinct >> Seq.length)
/// Solves Part 2 of the puzzle using the provided scoring function and input grid.
/// This function computes the score of trails in a grid, starting with trail heads ('0')
/// and following trails in numeric order ('0' -> '1' -> '2' ... '9').
///
/// It uses the `findTrailTails` function to recursively determine valid tail paths for trails.
/// For each trail head, it applies the `trailTailScorer` function to compute the score of its tail.
///
/// Parameters:
///   - trailTailScorer: Function determining how to score the tails of trails.
///   - input: The grid input (array of strings) used to calculate trail scores.
///
/// Returns:
///   The total score of all trails computed by the `trailTailScorer`.
let part2 = solve Seq.length

/// Executes the primary logic for solving both parts of the puzzle by applying
/// the part1 and part2 functions to the input data. The input data is read from
/// the file using the helper function runReadAllLines.
let run = runReadAllLines part1 part2

/// Provides unit tests for various functions and modules defined within the codebase.
module tests =
    open Swensen.Unquote
    open Xunit

    /// Contains the solution and utilities for solving the Day 10 challenge.
    let example =
        [| "89010123"
           "78121874"
           "87430965"
           "96549874"
           "45678903"
           "32019012"
           "01329801"
           "10456732" |]

    /// Represents a module related to Problem Day 10 functionalities and operations.
    [<Fact>]
    let ``Part 1 example`` () = part1 example =! 36

    /// Defines computational logic for Day 10 challenges.
    [<Fact>]
    let ``Part 2 example`` () = part2 example =! 81
