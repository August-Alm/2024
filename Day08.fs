module AdventOfCode.Day08

open Gaussian
open Plane
open System.Collections.Generic
open System.IO

let parse path =
  let map = Plane.parse id (File.ReadAllLines path)
  let antennas =
    Plane.positions map
    |> Seq.filter (fun pos -> Plane.get map pos <> '.')
    |> Seq.groupBy (Plane.get map)
    |> Seq.map (fun (_, group) -> Seq.toArray group)
  map, antennas

let simple (map, antennas) =
  let antinodes = HashSet<Gaussian> ()
  for set in antennas do
    for pos in set do
      for other in set do
        if pos <> other then
          antinodes.Add (pos + pos - other) |> ignore
  antinodes
  |> Seq.filter (Plane.contains map)
  |> Seq.length

let resonant (map, antennas) =
  let antinodes = HashSet<Gaussian> ()
  for set in antennas do
    for pos in set do
      for other in set do
        if pos <> other then
          let freq = pos - other
          let mutable node = pos
          while Plane.contains map node do
            antinodes.Add node |> ignore
            node <- node + freq
  antinodes
  |> Seq.filter (Plane.contains map)
  |> Seq.length


module Puzzle1 =
  
  let solve = parse >> simple


module Puzzle2 =
  
  let solve = parse >> resonant