module AdventOfCode.Day01

module Puzzle1 =

  open System
  open System.IO

  let solve (path : string) =
    let buf1 = ResizeArray 1000
    let buf2 = ResizeArray 1000
    use reader = new StreamReader (path)
    while not reader.EndOfStream do
      let line = reader.ReadLine ()
      let _, x = Int32.TryParse (line.AsSpan (0, 5))
      let _, y = Int32.TryParse (line.AsSpan (8, 5))
      buf1.Add x
      buf2.Add y
    let arr1 = buf1.ToArray ()
    let arr2 = buf2.ToArray ()
    Array.sortInPlace arr1
    Array.sortInPlace arr2
    (arr1, arr2)
    ||> Array.fold2 (fun acc x y -> acc + (abs (x - y))) 0


module Puzzle2 =

  open System
  open System.IO
  open System.Collections.Generic
  open System.Runtime.InteropServices

  type CountDictionary (count : int) =
    let dict = Dictionary<int, int> (count)

    member _.Add (key : int) =
      let mutable exists = false
      let r = &CollectionsMarshal.GetValueRefOrAddDefault (dict, key, &exists)
      //if exists then r <- r + 1 else r <- 1
      r <- r + 1
    
    member _.Similarity (key : int) =
      match dict.TryGetValue key with
      | true, v -> key * v
      | _ -> 0

  let solve (path : string) =
    let buff = ResizeArray 1000
    let dict = CountDictionary 1000
    use reader = new StreamReader (path)
    while not reader.EndOfStream do
      let line = reader.ReadLine ()
      let _, x = Int32.TryParse (line.AsSpan (0, 5))
      let _, y = Int32.TryParse (line.AsSpan (8, 5))
      buff.Add x
      dict.Add y
    let mutable similarity = 0
    for x in buff do
      similarity <- similarity + dict.Similarity x
    similarity
    

