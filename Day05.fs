module AdventOfCode.Day05

open System.Collections.Generic
open System.IO

type PageOrder () =
  let lesser = Dictionary<int, HashSet<int>> ()
  let greater = Dictionary<int, HashSet<int>> ()

  member _.AddOrdering (x : int) (y : int) =
    match greater.TryGetValue x with
    | true, set -> set.Add y |> ignore
    | _ -> greater.Add (x, HashSet [y])
    match lesser.TryGetValue y with
    | true, set -> set.Add x |> ignore
    | _ -> lesser.Add (y, HashSet [x])
  
  member _.Compare (x : int) (y : int) =
    match greater.TryGetValue x with
    | true, set when set.Contains y -> -1
    | _ ->
      match lesser.TryGetValue y with
      | true, set when set.Contains x -> 1
      | _ ->
        match greater.TryGetValue y with
        | true, set when set.Contains x -> 1
        | _ ->
          match lesser.TryGetValue x with
          | true, set when set.Contains y -> -1
          | _ -> 0


[<RequireQualifiedAccess>]
module PageOrder =

  let private addOrdering (order : PageOrder) (line : string) =
    if System.String.IsNullOrEmpty line then
      false
    else
      let xs = line.Split '|' |> Array.map int
      order.AddOrdering xs[0] xs[1]
      true
  
  let read (reader : StreamReader) =
    let order = PageOrder ()
    while not reader.EndOfStream && addOrdering order (reader.ReadLine ()) do ()
    order


type Update = int array

[<RequireQualifiedAccess>]
module Update =

  let read (reader : StreamReader) =
    let line = reader.ReadLine ()
    Array.map int (line.Split ',')

  let middle (update : Update) =
    update[update.Length / 2]
  
  let sort (order : PageOrder) (update : Update) =
    Array.sortWith order.Compare update

  let equal (update1 : Update) (update2 : Update) =
    Array.forall2 (fun x y -> x = y) update1 update2


module Puzzle1 =

  let solve (path : string) =
    let mutable count = 0
    use reader = new StreamReader (path)
    let order = PageOrder.read reader
    while not reader.EndOfStream do
      let update = Update.read reader
      let sorted = Update.sort order update
      if Update.equal update sorted then
        count <- count + Update.middle update
    count


module Puzzle2 =

  let solve (path : string) =
    let mutable count = 0
    use reader = new StreamReader (path)
    let order = PageOrder.read reader
    while not reader.EndOfStream do
      let update = Update.read reader
      let sorted = Update.sort order update
      if not (Update.equal update sorted) then
        count <- count + Update.middle sorted
    count