module AdventOfCode.Plane

open Gaussian

type Plane<'a> = 'a array2d

[<RequireQualifiedAccess>]
module Plane =

  let width = Array2D.length2

  let height = Array2D.length1

  let inline get (plane : Plane<'a>) (pos : Gaussian) =
    plane[pos.Y, pos.X]
  
  let inline set (plane : Plane<'a>) (pos : Gaussian) (value : 'a) =
    plane[pos.Y, pos.X] <- value
  
  let contains (plane : Plane<'a>) (pos : Gaussian) =
    (pos.Y >= 0 && pos.Y < height plane) &&
    (pos.X >= 0 && pos.X < width plane)
  
  let row (plane : Plane<'a>) (row : int) =
    Seq.init (width plane) (fun col -> plane[row, col])
  
  let column (plane : Plane<'a>) (col : int) =
    Seq.init (height plane) (fun row -> plane[row, col])
  
  let map (f : 'a -> 'b) (plane : Plane<'a>) =
    Array2D.map f plane
  
  let iter (f : 'a -> unit) (plane : Plane<'a>) =
    Array2D.iter f plane
  
  let positions (plane : Plane<'a>) =
    seq {
      for row in 0 .. height plane - 1 do
        for col in 0 .. width plane - 1 do
          yield { X = col; Y = row } }
  
  let parse (parser : char -> 'a) (lines : string array) =
    Array2D.init
      (lines.Length) lines[0].Length
      (fun row col -> parser (lines[row][col]))