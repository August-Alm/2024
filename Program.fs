﻿module AdventOfCode.Program

open AdventOfCode
open System.IO

[<EntryPoint>]
let main _ =

  let input i = Path.Combine ("Input", sprintf "Day%02i" i, $"puzzle.txt")
  let test i = Path.Combine ("Input", sprintf "Day%02i" i, $"test.txt")

  printfn "Day 01, Puzzle 1: %A" (Day01.Puzzle1.solve (input 1))
  printfn "Day 01, Puzzle 2: %A" (Day01.Puzzle2.solve (input 1))

  printfn "Day 02, Puzzle 1: %A" (Day02.Puzzle1.solve (input 2))
  printfn "Day 02, Puzzle 2: %A" (Day02.Puzzle2.solve (input 2))

  0