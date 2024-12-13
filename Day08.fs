/// Contains implementations and utilities for Day 08 of Advent of Code.
module aoc24.Day08

/// Represents a point in a 2D space as a struct of two integers.
type Point = (struct (int * int))

/// Represents a point in a 2D coordinate system.
/// Defined as a struct tuple of two integers (x, y).
module Point =
    /// Represents a point with integer coordinates `x` and `y`.
    let inline op f ((a, b): Point) ((c, d): Point) = struct (f a c, f b d)

/// Contains the logic and functions to solve Day08 specific tasks
let inline (.+) a b = Point.op (+) a b
/// Represents the module containing functionality for Advent of Code, Day 08.
let inline (.-) a b = Point.op (-) a b

/// Represents a grid initialized from a given input of string array. Provides methods for finding and processing grid elements.
type Grid(input: string array) =
    /// Provides utility functions and types for solving the Advent of Code Day 08 problem.
    let bx, by = (input.[0].Length, input.Length)

    /// Identifies all antennas within a grid and yields their coordinates.
    /// Antennas are located in a 2D grid represented as a string array, where each character corresponds to a cell.
    /// Non-antenna cells are represented as '.', while antenna cells are represented by other characters.
    /// Returns a sequence of tuples where the first element is the antenna identifier (character)
    /// and the second element is its position as a `Point` (x, y).
    member x.findAntennas =
        seq {
            for y = 0 to input.Length - 1 do
                for x = 0 to input.[y].Length - 1 do
                    match input.[y].[x] with
                    | '.' -> ()
                    | c -> yield (c, Point(x, y))
        }

    /// Groups the detected antennas based on their unique characters.
    /// This function identifies antenna positions in the grid, groups them
    /// by their corresponding unique identifiers, and returns a list of lists
    /// where each sub-list contains the positions of antennas with the same identifier.
    member x.groupedAntennas =
        x.findAntennas
        |> List.ofSeq
        |> List.groupBy fst
        |> List.map (snd >> List.map snd)

    /// Checks whether a given point is within the bounds of the grid.
    ///
    /// Parameters:
    ///   (x, y): The coordinates of the point to evaluate.
    ///
    /// Returns:
    ///   A boolean indicating whether the point is within the defined boundaries of the grid.
    member x.inBounds((x, y): Point) = x >= 0 && y >= 0 && x < bx && y < by

/// Generates all unique pairs of elements from a given list, filtering out pairs where both elements are identical.
/// Returns the pairs as a list.
let allPairs1 list =
    Seq.allPairs list list |> Seq.filter (fun (a, b) -> a <> b) |> List.ofSeq

/// Represents the solution logic for part 1 of the Day 08 problem of Advent of Code 2024.
///
/// Parses the input data and generates a grid.
/// Derives all valid antenna positions by iterating through the antenna pairs and calculating potential anti-nodes.
/// Filters the valid anti-nodes that lie within the grid bounds and counts the distinct ones.
///
/// Parameters:
///  - `input`: An array of strings representing the grid.
///
/// Returns:
///  - An integer indicating the count of valid distinct anti-nodes within the grid bounds.
let part1 input =
    let grid = input |> Grid

    let firstGenAntiNodes (a, b) =
        let vec = b .- a
        [ b .+ vec; a .- vec ]

    let antiNodes =
        grid.groupedAntennas
        |> List.collect (allPairs1 >> List.map firstGenAntiNodes)
        |> List.collect id

    antiNodes |> List.distinct |> List.filter grid.inBounds |> List.length

/// Calculates the solution for "part2" of the problem using a grid representation and antenna node processing.
///
/// Parameters:
///  - `input`: An array of strings representing the grid.
///
/// Returns:
///  - The count of distinct 'anti-nodes' within the bounds of the grid.
///
/// Implementation Details:
///  - A `Grid` is initialized using the input representation.
///  - Identifies pairs of coordinates within groups of antennas.
///  - Calculates potential 'anti-nodes' within bounds for each pair of coordinates, using vector operations.
///  - Aggregates and filters to find all distinct valid anti-nodes, ensuring they are within the grid bounds.
///
/// This method leverages custom vector operations, bounds checking, and sequence processing to compute the result.
let part2 input =
    let grid = input |> Grid

    let antiNodesInBounds (a, b) =
        let vec = b .- a

        let find op =
            List.unfold (function
                | p when grid.inBounds p -> Some(p, op p vec)
                | _ -> None)

        find (.+) b @ find (.-) a

    let antiNodes =
        grid.groupedAntennas
        |> List.collect (allPairs1 >> List.map antiNodesInBounds)
        |> List.collect id

    antiNodes |> List.distinct |> List.length

/// Entry point for executing solutions for Part 1 and Part 2 of the Day 08 problem.
let run = runReadAllLines part1 part2

/// Contains unit tests for the `Day08` module.
module tests =
    open Swensen.Unquote
    open Xunit

    /// Contains the functionality for solving Day 08 of the AoC24 challenge.
    let example1 =
        [| "............"
           "........0..."
           ".....0......"
           ".......0...."
           "....0......."
           "......A....."
           "............"
           "............"
           "........A..."
           ".........A.."
           "............"
           "............" |]

    /// Represents a solution to the Advent of Code Day 8 challenge.
    [<Fact>]
    let ``Part 1 example`` () = part1 example1 =! 14

    /// Represents a module for solving Day 08's problem in AoC 24.
    [<Fact>]
    let ``Part 2 example`` () = part2 example1 =! 34
