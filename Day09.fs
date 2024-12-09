module AdventOfCode.Day09

[<Struct>]
type Range (min : int, max : int) =
  member _.Min = min
  member _.Max = max
  member _.Length = max - min + 1

[<Struct>]
type File = { Min : int; Length : int }

type Disk (blocks : int voption array, allocated : File seq, free : Range seq) =
  let allocated = allocated |> Seq.toArray
  let mutable free = ResizeArray free
  member _.Blocks = blocks
  member _.Allocated = allocated
  member _.Free = free

[<RequireQualifiedAccess>]
module Disk =

  let inline private toDigit (c : char) =
    int c - int '0'

  let checksum (disk : Disk) =
    let inline zeroIfNone xopt =
      match xopt with
      | ValueSome x -> x
      | ValueNone -> 0
    disk.Blocks
    |> Array.mapi (fun i file -> int64 (i * zeroIfNone file))
    |> Array.sum

  let parse (map : string) =
    let volume = Seq.sumBy toDigit map
    let blocks = Array.zeroCreate volume
    let allocated = ResizeArray ()
    let free = ResizeArray ()
    let mutable file = -1
    let mutable head = 0
    for i = 0 to map.Length - 1 do
      let count = toDigit map[i]
      let empty = (i % 2 = 1)
      if not empty then
        file <- file + 1
        allocated.Add { Min = head; Length = count }
      elif count <> 0 then
        free.Add (Range (head, head + count - 1))
      for _ = 0 to count - 1 do
        blocks[head] <- if empty then ValueNone else ValueSome file
        head <- head + 1
    Disk (blocks, allocated, free)

  let defragmentBlocks (disk : Disk) =
    let mutable head = 0
    let mutable tail = disk.Blocks.Length - 1
    while head < tail do
      if disk.Blocks[head].IsSome then
        head <- head + 1
      else
        while disk.Blocks[tail].IsNone do
          tail <- tail - 1
        disk.Blocks[head] <- disk.Blocks[tail]
        head <- head + 1
        disk.Blocks[tail] <- ValueNone
        tail <- tail - 1
    checksum disk
  
  let defragmentFiles (disk : Disk) =
    for fileId = disk.Allocated.Length - 1 downto 0 do
      let file = disk.Allocated[fileId]
      let mutable j = 0
      let mutable todo = true
      while todo && j < disk.Free.Count do
        let free = disk.Free[j]
        if free.Min > file.Min then
          todo <- false
        elif free.Length < file.Length then
          j <- j + 1 // continue
        else
          for k = 0 to file.Length - 1 do
            disk.Blocks[free.Min + k] <- ValueSome fileId
            disk.Blocks[file.Min + k] <- ValueNone
          disk.Free.RemoveAt j
          if free.Length > file.Length then
            disk.Free.Insert (j, Range (free.Min + file.Length, free.Max))
          todo <- false
    checksum disk


module Puzzle1 =

  let solve (path : string) =
    Disk.parse (System.IO.File.ReadAllText path)
    |> Disk.defragmentBlocks
  
module Puzzle2 =

  let solve (path : string) =
    Disk.parse (System.IO.File.ReadAllText path)
    |> Disk.defragmentFiles
