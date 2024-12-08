module AdventOfCode.Day06

open System.Collections.Generic
open Gaussian

[<RequireQualifiedAccess>]
module Dir =

  let up = -Gaussian.One
  let turn dir = Gaussian.rotateR dir


type Lab (path : string) =
  let mutable rows = 0
  let mutable cols = 0
  let mutable guard = Gaussian.Zero
  let obstacles = HashSet<Gaussian> ()

  do
    let lines = System.IO.File.ReadAllLines path
    rows <- lines.Length
    cols <- lines[0].Length
    for i in 0 .. rows - 1 do
      for j in 0 .. cols - 1 do
        match lines[i][j] with
        | '#' -> obstacles.Add { X = i; Y = j } |> ignore
        | '^' -> guard <- { X = i; Y = j }
        | _ -> ()
  
  member _.Guard = guard
  
  member _.IsObstructed (z : Gaussian) = obstacles.Contains z

  member _.Contains (z : Gaussian) =
    (z.Y >= 0 && z.Y < cols) && (z.X >= 0 && z.X < rows)


let run (lab : Lab) =
  let visited = HashSet<Gaussian> ()
  let mutable guard = lab.Guard
  let mutable dir = Dir.up
  while lab.Contains guard do
    visited.Add guard |> ignore
    let next = guard + dir
    if lab.Contains next && lab.IsObstructed next then
      dir <- Dir.turn dir
    else
      guard <- next
  visited

let private check (lab : Lab) (obstacle : Gaussian) =
  let mutable pos = lab.Guard
  let mutable dir = Dir.up
  let visited = HashSet<struct (Gaussian * Gaussian)> ()
  while visited.Add (pos, dir) && lab.Contains (pos + dir) do
    let next = pos + dir
    if next = obstacle || lab.IsObstructed next then
      dir <- Dir.turn dir
    else
      pos <- next
  if lab.Contains (pos + dir) then 1 else 0

let loops (lab : Lab) =
  let path = run lab
  path.Remove lab.Guard |> ignore
  Array.Parallel.sumBy (check lab) (Array.ofSeq path)


module Puzzle1 =

  let solve = Lab >> run >> Seq.length


module Puzzle2 =

  let solve = Lab >> loops
