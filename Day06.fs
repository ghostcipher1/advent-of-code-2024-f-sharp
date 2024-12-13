/// Module representing Day06 functionalities for Advent of Code.
module aoc24.Day06

open System.Collections.Generic

/// Represents a point in a 2D grid as a tuple of two integers.
type Point = (struct (int * int))

/// Represents a cursor in a grid that has a position and a direction.
[<Struct>]
type Cursor = { 
/// Represents the position of a cursor or point as a tuple of integers.
position: Point; 
/// Represents a namespace for solving Day06 of Advent of Code.
direction: Point }

/// Represents a 2D point as a struct with two integer coordinates.
module Point =
    /// This module represents the solution for Day 06 of Advent of Code 2024.
    /// It provides logic for processing grid-based inputs and simulating cursor movements on the grid.
    let inline op f ((a, b): Point) ((c, d): Point) = struct (f a c, f b d)

/// The Day06 module provides logic for solving Advent of Code Day 6 challenges.
/// It includes definitions of key types, utility functions for grid manipulation,
/// pathfinding logic for navigating a grid, and solving the described puzzles.
let inline (.+) a b = Point.op (+) a b
/// A point in 2D space represented as a tuple of two integers.
let inline (.-) a b = Point.op (-) a b

/// Provides utilities for grid manipulation and operations.
module Grid =
    /// Represents a 2D point with integer coordinates.
    let inline at struct (x, y) (grid: string array) = grid[y][x]

    /// Checks the bounds of the input grid and returns the character at the specified position if valid.
    /// If the position is out of bounds, returns a space character (' ').
    ///
    /// Parameters:
    ///   - x, y: The coordinates (x: column, y: row) of the position to access on the grid.
    ///   - grid: A 2D grid represented as a string array where each element is a row of the grid.
    ///
    /// Returns:
    /// The character at the specified position if the position exists within the bounds of the input grid.
    /// Otherwise, returns a space character (' ').
    ///
    /// Example input grid:
    /// [
    ///   "###",
    ///   "#.#",
    ///   "###"
    /// ]
    ///
    /// Example:
    /// - For `tryAt struct (1, 1) grid`, returns `'.'`.
    /// - For `tryAt struct (-1, 0) grid`, returns `' '`.
    let inline tryAt (struct (x, y)) (grid: string array) =
        if y >= 0 && y < grid.Length && x >= 0 && x < grid[y].Length then
            grid[y][x]
        else
            ' '

    /// Generates a sequence of all possible grid positions as a structured tuple (x, y).
    /// The sequence is constructed by iterating through each row and then through each column for every row.
    /// grid: A 2D grid represented as an array of strings.
    /// Returns a sequence of Point structures where each represents a position in the grid.
    let allPos (grid: string array) =
        seq {
            for y = 0 to grid.Length - 1 do
                for x = 0 to grid[y].Length - 1 do
                    yield struct (x, y)
        }

    /// Finds all positions of a given character in a grid.
    ///
    /// Parameters:
    ///  - `chr`: The character to find.
    ///  - `grid`: The grid to search in, represented as a string array.
    ///
    /// Returns:
    ///  - A list of positions, represented as `Point` structures, where the specified character occurs in the grid.
    let findChars chr grid =
        grid |> allPos |> Seq.filter (fun p -> grid |> at p = chr) |> List.ofSeq


/// Represents a struct type defining a cursor's position and movement direction.
module Cursor =
    /// A module containing main functionality for solving the Day 06 problem.
    /// Includes structures, helper functions, and logic to simulate cursor movement
    /// on a grid.
    let directions =
        [ struct (0, -1) // N
          struct (1, 0) // E
          struct (0, 1) // S
          struct (-1, 0) ] // W

    /// Turns the cursor to face the next direction in a clockwise manner.
    ///
    /// The function identifies the current direction of the cursor and returns a new cursor
    /// with its direction updated to the next direction in the `directions` list.
    /// Directions are ordered as follows: North, East, South, West.
    let turnRight cursor =
        let index = directions |> List.findIndex (fun i -> i = cursor.direction)

        { cursor with
            direction = directions |> List.item ((index + 1) % 4) }

    /// Moves the `Cursor` forward based on its current position and direction.
    /// Returns a new `Cursor` instance with the updated position.
    let walk cursor =
        { cursor with
            position = cursor.position .+ cursor.direction }

    ///
    /// Reverses the walking direction of a cursor in the grid, making it move backward
    /// along its current direction. This function computes the new position of the
    /// cursor by subtracting its current direction vector from its position.
    ///
    /// Arguments:
    /// - `cursor`: The `Cursor` instance representing the current position and direction.
    ///
    /// Returns:
    /// A new `Cursor` object representing the position of the cursor one step back
    /// along the direction it is currently facing.
    let walkBack cursor =
        { cursor with
            position = cursor.position .- cursor.direction }

/// Computes the initial cursor position and direction for the input grid.
/// The function identifies the point on the grid that contains the '^' character
/// and sets the initial cursor position to that point. It also initializes the
/// cursor's direction to the first direction in `Cursor.directions`.
///
/// Parameters:
///  - input: An array of strings representing the grid.
///
/// Returns:
///  - A `Cursor` object containing the starting position and the initial direction.
let getStartCursor input =
    let start = input |> Grid.findChars '^' |> Seq.exactlyOne

    { position = start
      direction = Cursor.directions.Head }

/// Generates a sequence of cursor movements based on the given input grid and the starting cursor position.
/// The cursor moves along the path adhering to the grid rules:
/// - Moves forward when encountering '^' or '.'.
/// - If it encounters a wall '#', it backtracks and turns right.
/// - Stops when encountering a space ' ' or an unexpected character in the input grid.
/// - Unexpected input raises an error.
let walkPath cursor input =
    cursor
    |> List.unfold (fun state ->
        match input |> Grid.tryAt state.position with
        | ('^' | '.') -> Some(state, state |> Cursor.walk)
        | '#' ->
            let backtracked = state |> Cursor.walkBack
            Some(backtracked, backtracked |> Cursor.turnRight)
        | ' ' -> None
        | c -> failwith $"unexpected input: {c}")

/// A module containing solutions for Day 06 of Advent of Code.
let part1 input =
    let cursor = getStartCursor input
    let path = walkPath cursor input

    path |> List.distinctBy _.position |> List.length


///
/// Represents the end result of a simulation state.
///
type SimulationResult =
    /// Represents the result of a simulation, either a loop or an exit.
    | Loop
    ///
    /// Contains functionality for parsing and processing a grid-based input,
    /// including operations on points, grids, cursors, and solving specific tasks.
    | Exit

/// Computes the result for Part 2 of the Day 6 challenge.
/// Simulates the behavior of a cursor navigating a grid under constrained rules.
/// Adds obstruction points dynamically during simulation to detect loops and exits.
///
/// Parameters:
/// - `input`: The grid representation as a string array.
///
/// Returns:
/// The count of guard states resulting in loops after introducing obstructions.
///
/// Highlights:
/// - Generates initial cursor-path pairs dynamically for identifying potential obstructions.
/// - Simulates navigation behavior using `Cursor` and grid manipulation by tracking all visited turns.
/// - Handles simulation of two possible outcomes, `Loop` and `Exit`.
/// - Parallelizes simulation for performance optimization.
let part2 input =
    let simulate cursor addedObstruction =
        // Store only visited turns with the direction the guard is facing in. This is enough to detect a loop.
        let visitedTurns = HashSet<Cursor>()

        let rec loop state =
            let atPosition =
                if state.position = addedObstruction then
                    '#'
                else
                    input |> Grid.tryAt state.position

            if atPosition <> '#' || visitedTurns.Add state then
                match atPosition with
                | ('^' | '.') -> loop (state |> Cursor.walk)
                | '#' -> loop (state |> Cursor.walkBack |> Cursor.turnRight)
                | ' ' -> Exit
                | c -> failwith $"unexpected input: {c}"
            else
                Loop

        loop cursor

    let startCursorAddedObstructionPairs =
        walkPath (getStartCursor input) input
        |> Seq.distinctBy _.position
        |> Seq.pairwise
        |> Seq.map (fun (first, second) -> (first, second.position))
        |> Seq.toArray

    let simulationResults =
        startCursorAddedObstructionPairs
        |> Array.Parallel.map (fun (cursor, obstruction) -> simulate cursor obstruction)

    simulationResults |> Array.filter _.IsLoop |> Array.length


/// Executes the Day06 problem solution by running part1 and part2 on the input data.
/// Utilizes `runReadAllLines` to read the input and invoke appropriate processing functions
/// for both parts of the solution.
let run = runReadAllLines part1 part2

/// Unit tests module for validating the functionality of Day 06 puzzles.
module tests =
    open Swensen.Unquote
    open Xunit

    /// Module implementing day 6 of Advent of Code 2024 challenge.
    let example =
        [| "....#....."
           ".........#"
           ".........."
           "..#......."
           ".......#.."
           ".........."
           ".#..^....."
           "........#."
           "#........."
           "......#..." |]

    /// Defines a module containing the solution for Day 06 of Advent of Code 2024.
    [<Fact>]
    let ``Part 1 example`` () = part1 example =! 41

    /// Represents the module containing functions and types for solving Day 06 of Advent of Code.
    [<Fact>]
    let ``Part 2 example`` () = part2 example =! 6
