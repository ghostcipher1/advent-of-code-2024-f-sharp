open System
open System.IO
open System.Net.Http
open System.Reflection
open System.Threading.Tasks
open FSharp.Text.RegexProvider
open aoc24

[<assembly: Xunit.CaptureTrace>]
[<assembly: Xunit.CaptureConsole>]
do ()

/// Represents the year for which the Advent of Code tasks are being solved.
let year = "2024"
/// Represents the directory location where input files for the program are stored.
let inputDirectory = "inputs"

/// Combines the input directory path with a file name for a specific day's input file.
/// The file name is constructed according to the pattern "dayXX.txt", where "XX" is the day number.
/// The path is built using the input directory and the constructed file name.
///
/// day: The integer representation of the day for which the input file path is required.
///
/// Returns the full path to the input file for the specified day.
let inputPath day =
    Path.Combine(inputDirectory, $"day%02u{day}.txt")

/// Downloads input files for all days up to the specified maximum day
/// for a given year's Advent of Code challenge. Files are downloaded
/// from the Advent of Code website and saved into a specified input
/// directory. Downloads are skipped for days when a file already exists.
///
/// This function relies on an AOC_SESSION environment variable, which
/// must contain a valid Advent of Code session key. If the session variable
/// is unset or invalid, the function skips downloading and logs a message.
///
/// A .env file can be used to load the session key into the environment
/// variables automatically. The input files are saved in the "inputs"
/// directory, with filenames in the format `dayXX.txt`. If the input
/// directory does not exist, it will be created.
///
/// maxDay: The maximum day for which input files should be downloaded (inclusive).
/// Returns: A Task that represents the asynchronous download operation.
let downloadAllDays maxDay =

    let dotEnv path =
        if File.Exists path then
            let vars =
                File.ReadAllLines(path)
                |> Array.map (StringEx.splitC '=')
                |> Array.choose (function
                    | [| key; value |] -> Some(key, value)
                    | _ -> None)

            for key, value in vars do
                Environment.SetEnvironmentVariable(key, value)

    dotEnv ".env"
    Directory.CreateDirectory(inputDirectory) |> ignore

    let httpClient =
        let handler = new HttpClientHandler(UseCookies = false)
        let client = new HttpClient(handler)
        let sessionKey = Environment.GetEnvironmentVariable("AOC_SESSION")
        client.DefaultRequestHeaders.Add("Cookie", $"session={sessionKey}")
        client

    let downloadInput day =
        printf $"Downloading input for Day {day}... "

        task {
            let! response = httpClient.GetAsync($"https://adventofcode.com/{year}/day/{day}/input")
            response.EnsureSuccessStatusCode |> ignore
            use stream = response.Content.ReadAsStream()
            use fileStream = File.OpenWrite(inputPath day)
            stream.CopyTo(fileStream)
            printfn "done"
        }

    task {
        if Environment.GetEnvironmentVariable("AOC_SESSION") |> String.IsNullOrWhiteSpace then
            printf "Skipping input download, because AOC_SESSION is unset"
        else
            for day = 1 to maxDay do
                if not (File.Exists(inputPath day)) then
                    do! downloadInput day
    }

/// Represents a strongly-typed regex for matching strings of the format "Day<num>".
/// Captures the numerical portion of the day (e.g., "Day12" captures "12").
/// Designed to parse types associated with specific day-based challenges or modules in the context
/// of the larger application.
type DayRegex = Regex< @"Day(?<num>\d+)" >

/// Represents a list of functions or tasks, one for each day, corresponding to the Advent of Code challenges.
/// Each function processes the input for its respective day based on its implemented logic.
///
/// The functions are created by dynamically finding and invoking static `run` methods within types.
/// These types are identified through reflection by searching for specific naming patterns in the current assembly.
let days =

    let dayModules =
        Assembly.GetExecutingAssembly().GetTypes()
        |> Array.filter (fun t ->
            t.IsClass
            && t.IsAbstract
            && t.IsSealed
            && t.IsPublic
            && (t.DeclaringType = null)) // Ensure it's a top-level module
        |> Array.choose (fun t ->
            DayRegex().TryTypedMatch(t.Name)
            |> Option.map (fun m -> (m.num.Value |> int, t)))
        |> Map

    let createDay i =
        dayModules
        |> Map.tryFind i
        |> Option.map (fun t ->

            let propertyMethodInfo =
                t.GetMethod("get_run", BindingFlags.Public ||| BindingFlags.Static)

            let functionMethodInfo =
                t.GetMethod("run", BindingFlags.Public ||| BindingFlags.Static)

            if (propertyMethodInfo = null && functionMethodInfo = null) then
                failwith $"Type {t.Name} doesn't have a method 'run'"

            if (propertyMethodInfo <> null) then
                (fun () ->
                    let run = propertyMethodInfo.Invoke(null, [||]) :?> (string -> string)
                    run (inputPath i))
            else
                (fun () -> functionMethodInfo.Invoke(null, [| inputPath i |]) :?> string))

        |> Option.defaultValue (fun () -> "skipped")

    let maxDay = dayModules |> Map.keys |> Seq.max
    [ 1..maxDay ] |> List.map createDay

/// Executes all "day" implementations sequentially from the `days` list.
/// Each day's implementation is executed as a task, and results are printed in reverse order.
/// The day index is matched to its corresponding result.
let runAll () =

    let tasks =
        days
        |> List.indexed
        |> List.rev
        |> List.map (fun (i, r) ->
            task {
                let! result = Task.Run r
                return (i, result)
            })

    task {
        for task in tasks do
            let! i, result = task
            printfn $"Day {i + 1} {result}"
    }
    |> Task.WaitAll

open TryParser

/// Entry point of the application.
///
/// This function orchestrates the execution of the program. It downloads input files for all available Advent of Code days and processes days based on user-provided parameters:
/// - A specific day's implementation, if found, will be run.
/// - If "latest" is specified, the last implemented day will be run.
/// - If no parameters are provided, all implemented days are executed consecutively.
/// - Invalid or unsupported input produces an error.
///
/// Parameters:
///  - `args`: Command-line arguments passed by the user.
///     - A single number: Executes the corresponding day's implementation.
///     - "latest": Executes the latest implemented day's solution.
///     - No arguments: Executes all implemented days.
///     - Any other input results in an error.
///
/// Returns:
///  - Exit code of the program:
///     - `0` for successful execution.
///     - `1` for errors (e.g., missing implementations, invalid arguments).
[<EntryPoint>]
let Main args =

    (downloadAllDays days.Length).Wait()

    match args with
    | [| Int day |] when day > 0 && day <= 25 ->

        match days |> List.tryItem (day - 1) with
        | Some implementation ->
            printfn $"Running day {day}:"
            printfn $"{implementation ()}"
            0

        | None ->
            printfn $"Could not find an implementation for day {day}"
            1

    | [| "latest" |] when days.Length > 0 ->
        printfn $"Running latest (day {days.Length}):"
        printf $"{(days |> List.last) ()}"
        0

    | [||] ->
        printfn $"Running all days (1 - {days.Length})"
        runAll ()
        0

    | _ ->
        printf "Expect either a number, 'latest', no parameters"
        1
