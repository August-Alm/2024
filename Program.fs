module AdventOfCode.Program

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

  printfn "Day 03, Puzzle 1: %A" (Day03.Puzzle1.solve (input 3))
  printfn "Day 03, Puzzle 2: %A" (Day03.Puzzle2.solve (input 3))

  printfn "Day 04, Puzzle 1: %A" (Day04.Puzzle1.solve (input 4))
  printfn "Day 04, Puzzle 2: %A" (Day04.Puzzle2.solve (input 4))

  printfn "Day 05, Puzzle 1: %A" (Day05.Puzzle1.solve (input 5))
  printfn "Day 05, Puzzle 2: %A" (Day05.Puzzle2.solve (input 5))

  printfn "Day 06, Puzzle 1: %A" (Day06.Puzzle1.solve (input 6))
  printfn "Day 06, Puzzle 2: %A" (Day06.Puzzle2.solve (input 6))

  printfn "Day 07, Puzzle 1: %A" (Day07.Puzzle1.solve (input 7))
  printfn "Day 07, Puzzle 2: %A" (Day07.Puzzle2.solve (input 7))

  0