module AdventOfCode.Day02


[<AutoOpen>]
module Common =

  open System

  type State =
    | Init
    | First of int
    | Increasing of int
    | Decreasing of int

  type Result =
    | Safe
    | Unsafe

  let inline increasing (last : int) (curr : int) =
    let diff = curr - last in (diff > 0) && (diff < 4)
  
  let inline decreasing (last : int) (curr : int) =
    increasing curr last
  
  let inline split (report : string) =
    let ranges = Array.zeroCreate<Range> 10
    let span = report.AsSpan ()
    let n = span.Split (ranges, ' ')
    Array.take n ranges
  
  let inline integer (range : Range) (report : string) =
    let span = report.AsSpan ()
    let start = range.Start.Value
    let length = range.End.Value - start
    let slice = span.Slice (start, length)
    match Int32.TryParse slice with
    | true, x -> x
    | _ -> failwith "invalid integer"

  let checkRanges (ranges : Range array) (report : string) =
    let rec loop i state =
      if i = ranges.Length then
        match state with
        | Init -> failwith "empty report"
        | _ -> Safe
      else
        let x = integer ranges[i] report
        match state with
        | Init -> loop (i + 1) (First x)
        | First y when increasing y x -> loop (i + 1) (Increasing x)
        | First y when decreasing y x -> loop (i + 1) (Decreasing x)
        | Increasing y when increasing y x -> loop (i + 1) (Increasing x)
        | Decreasing y when decreasing y x -> loop (i + 1) (Decreasing x)
        | _ -> Unsafe
    loop 0 Init


module Puzzle1 =

  open System.IO

  let check report = checkRanges (split report) report

  let solve (path : string) =
    File.ReadAllLines path
    |> Array.sumBy (fun report ->
      match check report with
      | Safe -> 1
      | _ -> 0)


module Puzzle2 = 

  open System.IO

  let check report =
    let ranges = split report
    let rec loop i =
      if i = ranges.Length then Unsafe
      else
        match checkRanges (Array.removeAt i ranges) report with
        | Safe -> Safe
        | Unsafe -> loop (i + 1)
    loop 0

  let solve (path : string) =
    File.ReadAllLines path
    |> Array.sumBy (fun report ->
      match check report with
      | Safe -> 1
      | _ -> 0)

