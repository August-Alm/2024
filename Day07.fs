module AdventOfCode.Day07

open System.Collections.Generic
open System.IO
open System

[<Struct>]
type State = { Val : int64; Idx : int }
with
  static member advance v (s : State) =
    { s with Val = v; Idx = s.Idx + 1 }

let longs (line : string) =
  line.Split ' ' |> Array.mapi (fun i x ->
    int64 (if i = 0 then x.TrimEnd ':' else x))

let inline concatenate (a : int64) (b : int64) =
  let d = int64 (Math.Log10 (double b) + 1.0)
  a * int64 (Math.Pow (10, double d)) + b

let calibrate concat (reader : StreamReader) =
  let mutable total = 0L
  let queue = Queue<State> 32

  let rec loop (xs : int64 array) =
    if queue.Count > 0 then
      let s = queue.Dequeue ()
      if s.Idx >= xs.Length then
        if s.Val = xs.[0] then
          total <- total + xs[0]
        else
          loop xs
      elif s.Val > xs[0] then
        loop xs
      else
        queue.Enqueue (State.advance (s.Val + xs[s.Idx]) s)
        queue.Enqueue (State.advance (s.Val * xs[s.Idx]) s)
        if concat then
          queue.Enqueue (State.advance (concatenate s.Val xs[s.Idx]) s)
        loop xs

  while not reader.EndOfStream do
    let xs = longs (reader.ReadLine ())
    queue.Clear ()
    queue.Enqueue { Val = xs[1]; Idx = 2 }
    loop xs

  total


module Puzzle1 =

  let solve (path : string) =
    use reader = new StreamReader (path)
    calibrate false reader

module Puzzle2 =
  
    let solve (path : string) =
      use reader = new StreamReader (path)
      calibrate true reader

