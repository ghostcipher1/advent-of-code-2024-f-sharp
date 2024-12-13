///
/// Contains the main implementation for solving Day 09 of Advent of Code.
module aoc24.Day09

/// Represents a memory unit that can either hold a value (Some value) or have no value (None).
type Memory = int voption

/// Processes a given sequence of input data to generate a sequence of memory values.
/// This function transforms the input characters into numerical byte values, which
/// are then expanded into a sequence where each byte determines repeated memory allocations.
/// For even-indexed items, it produces `Memory.Some` values representing file IDs,
/// and for odd-indexed items, it produces `Memory.None`.
///
/// Parameters:
///   input: A sequence of input characters representing map data.
///
/// Returns:
///   A sequence of memory values (`Memory.Some` or `Memory.None`) based on the input map.
let unfoldDiskMap input =
    let map = input |> Seq.map (fun c -> c - '0' |> byte) |> Seq.toArray

    seq {
        for i = 0 to map.Length - 1 do
            for j = 1uy to map.[i] do
                match i % 2 with
                | 0 -> Memory.Some(i / 2)
                | _ -> Memory.None
    }

/// Provides extensions for working with sequences.
module SeqEx =
    /// Returns a sequence of tuples, where each tuple contains the index and the value
    /// of the corresponding element in the input sequence.
    ///
    /// seq: Input sequence to process.
    let valueIndexed (seq: 'a seq) =
        seq |> Seq.mapi (fun i v -> struct (i, v))

/// Represents a type that can optionally hold an integer memory value.
let part1 input =
    let unfolded = input |> unfoldDiskMap |> SeqEx.valueIndexed |> Array.ofSeq

    use reverseEnumerator =
        unfolded
        |> Seq.rev
        |> Seq.choose (function
            | i, Memory.ValueSome v -> Some(i, v)
            | _ -> None)
        |> _.GetEnumerator()

    reverseEnumerator.MoveNext() |> ignore

    unfolded
    |> Seq.sumBy (fun struct (i, fileId) ->
        let revi, revFileId = reverseEnumerator.Current

        match fileId with
        | _ when i > revi -> 0
        | Memory.ValueSome fileId -> i * fileId
        | Memory.ValueNone ->
            reverseEnumerator.MoveNext() |> ignore
            i * revFileId
        |> int64)

/// Represents a memory block in a disk map layout, which can either be a stored file or an empty space.
[<Struct>]
type MemoryBlock =
    /// The Day09 module contains the solution for the Advent of Code - Day 09 problem.
    | File of id: int * length: int
    /// The `Day09` module provides implementations of solutions to the problems of Day 09.
    /// It includes functionalities related to unfolding disk maps, evaluating memory structures,
    /// and computing results based on input data.
    | Space of length: int

/// Given an input string representation of disk memory, this function calculates
/// a specific result based on reordering operations involving files and spaces
/// in the memory.
///
/// The input is a sequence of characters representing the memory layout, where
/// even-indexed characters correspond to files with their lengths, and odd-indexed
/// characters represent spaces with their lengths.
///
/// The function processes the memory, identifies files and spaces, and attempts
/// to reorder files into compatible spaces. It calculates the sum of products of
/// file IDs and their respective positions post-reordering.
///
/// Returns the computed result as an int64 value.
let part2 input =

    let memory =
        let map = input |> Seq.map (fun c -> c - '0' |> int) |> Seq.toArray

        (0, map |> SeqEx.valueIndexed)
        ||> Seq.mapFold (fun offset struct (i, v) ->
            match i % 2 with
            | 0 -> (offset, File(id = i / 2, length = map.[i]))
            | _ -> (offset, Space(length = map.[i]))
            , offset + map.[i])
        |> fst
        |> Seq.toList

    let files = memory |> List.filter (fun (_, v) -> v.IsFile)

    let reorderedMemory =
        (files, memory)
        ||> List.foldBack (fun (fpos, file) memory ->
            let matchingSpace =
                memory
                |> Seq.filter (fun (pos, _) -> pos < fpos)
                |> Seq.distinctBy fst
                |> Seq.tryFind (fun (spos, space) ->
                    match (space, file) with
                    | Space slen, File(length = flen) when spos < fpos && slen >= flen -> true
                    | _ -> false)

            match matchingSpace, file with
            | Some(spos, Space slen), File(length = flen) ->
                (spos, file)
                :: if slen > flen then
                       [ (spos + flen, Space(slen - flen)) ]
                   else
                       []
                @ (fpos, Space flen) :: memory

            | _ -> memory)

    reorderedMemory
    |> Seq.distinctBy fst
    |> Seq.sumBy (fun (pos, block) ->
        match block with
        | File(fid, len) -> ({ pos .. pos + len - 1 } |> Seq.sumBy (fun i -> int64 i * int64 fid))
        | Space _ -> 0)

/// Runs the solution for the given Advent of Code problem using the provided
/// input processing functions `part1` and `part2`, and the specified input file.
/// The function reads the content of the provided file and applies `part1` or
/// `part2` to compute results.
let run = runReadAllText part1 part2

/// Contains several modules, types, and functions for solving Day 09 of the Advent of Code challenge.
module tests =
    open Swensen.Unquote
    open Xunit

    /// Defines the namespace and module for Day09-specific implementations.
    let examples =
        [ "12345" //
          "2333133121414131402" ]

    /// Represents the expected outputs for Part 1 tests.
    let part1Expected =
        [ 60 //
          1928 ]
        |> List.zip examples

    /// Main module for solving Day 09 of Advent of Code.
    /// Contains functions and types required to perform the necessary operations.
    [<Theory; MemberData(nameof part1Expected)>]
    let ``Part 1 examples`` input expected = part1 input =! expected

    /// Contains functionality for solving "Day 09" challenges.
    [<Fact>]
    let ``Part 2 example`` () = part2 examples[1] =! 2858

    /// Represents a test function for parsing and unfolding disk map data.
    /// This function maps a sequence of input strings to their respective expected transformed outputs.
    /// The mapping uses `unfoldDiskMap` and maps the resulting sequence to a string representation
    /// where some file IDs are transformed, while others represent empty positions with a placeholder '.'.
    /// The function is designed to work as an input and expectation generator for unit tests.
    let parseUnfoldData =
        [ "0..111....22222" //
          "00...111...2...333.44.5555.6666.777.888899" ]
        |> List.zip examples

    /// Day09 module contains functions and types for solving Advent of Code 2023 Day 9.
    [<Theory; MemberData(nameof parseUnfoldData)>]
    let ``Test parse and unfold`` input expected =
        input
        |> unfoldDiskMap
        |> Seq.map (function
            | ValueSome fileId -> fileId.ToString()[0]
            | ValueNone -> '.')
        |> System.String.Concat
        =! expected
