module AdventOfCode.Day06

open System.Collections.Generic
open System.Numerics

[<RequireQualifiedAccess>]
module Dir =

  let up = -Complex.One
  let down = Complex.One
  let left = -Complex.ImaginaryOne
  let right = Complex.ImaginaryOne
  let turn dir = -Complex.ImaginaryOne * dir


type Lab (path : string) =
  let mutable rows = 0
  let mutable cols = 0
  let mutable guard = Complex.Zero
  let obstacles = HashSet<Complex> ()

  do
    let lines = System.IO.File.ReadAllLines path
    rows <- lines.Length
    if rows > 0 then
      cols <- lines[0].Length
      for i in 0 .. rows - 1 do
        for j in 0 .. cols - 1 do
          match lines[i][j] with
          | '#' -> obstacles.Add (Complex (i, j)) |> ignore
          | '^' -> guard <- Complex (i, j)
          | 'v' -> guard <- Complex (i, j)
          | '<' -> guard <- Complex (i, j)
          | '>' -> guard <- Complex (i, j)
          | _ -> ()
  
  member _.Guard = guard
  
  member _.IsObstructed (z : Complex) = obstacles.Contains z

  member _.Contains (z : Complex) =
    (z.Imaginary >= 0 && z.Imaginary < cols) &&
    (z.Real >= 0 && z.Real < rows)


[<RequireQualifiedAccess>]
module Lab =

  let run (lab : Lab) =
    let visited = HashSet<Complex> ()
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
  
  let private check (lab : Lab) (obstacle : Complex) =
    let mutable pos = lab.Guard
    let mutable dir = Dir.up
    let visited = HashSet<struct (Complex * Complex)> ()
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

  let solve (path : string) =
    (Lab.run (Lab path)).Count


module Puzzle2 =

  let solve (path : string) =
    Lab.loops (Lab path)
