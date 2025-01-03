module AdventOfCode.Day04

[<Struct>]
type Direction = NW | N | NE | W | E | SW | S | SE

[<Struct>]
type Position = { Col : int; Row : int }

[<RequireQualifiedAccess>]
module Position = 

  let inline add (pos : Position) (dir : Direction) =
    match dir with
    | NW -> { Col = pos.Col - 1; Row = pos.Row - 1 }
    | N  -> { Col = pos.Col;     Row = pos.Row - 1 }
    | NE -> { Col = pos.Col + 1; Row = pos.Row - 1 }
    | W  -> { Col = pos.Col - 1; Row = pos.Row }
    | E  -> { Col = pos.Col + 1; Row = pos.Row }
    | SW -> { Col = pos.Col - 1; Row = pos.Row + 1 }
    | S  -> { Col = pos.Col;     Row = pos.Row + 1 }
    | SE -> { Col = pos.Col + 1; Row = pos.Row + 1 }


type Input = char array array

[<RequireQualifiedAccess>]
module Input =

  let fromFile (path : string) =
    System.IO.File.ReadAllLines path
    |> Array.map (fun s -> s.ToCharArray ())
  
  let isWithin (inp : Input) (pos : Position) =
    (pos.Row >= 0 && pos.Row < inp.Length) &&
    (pos.Col >= 0 && pos.Col < inp[pos.Row].Length)
  
  let inline get (inp : Input) (pos : Position) =
    inp[pos.Row][pos.Col]
  
  let tryGet (inp : Input) (pos : Position) =
    if isWithin inp pos then Some (get inp pos) else None
  
  let positions (inp : Input) =
    let result = ResizeArray ()
    inp |> Array.iteri (fun i row ->
      row |> Array.iteri (fun j _ ->
        result.Add { Row = i; Col = j }))
    Array.ofSeq result
  

module Puzzle1 =

  let rec private spell letter inp pos dir =
    let go letter pos dir = spell letter inp (Position.add pos dir) dir
    match letter with
    | 'X' when Input.tryGet inp pos = Some 'X' -> go 'M' pos dir
    | 'M' when Input.tryGet inp pos = Some 'M' -> go 'A' pos dir
    | 'A' when Input.tryGet inp pos = Some 'A' -> go 'S' pos dir
    | 'S' when Input.tryGet inp pos = Some 'S' -> 1
    | _ -> 0

  let private xmasCount (inp : Input) (pos : Position) =
    Array.sumBy (spell 'X' inp pos) [| NW; N; NE; W; E; SW; S; SE |]
  
  let solve (path : string) =
    let inp = Input.fromFile path
    Array.sumBy (xmasCount inp) (Input.positions inp)


module Puzzle2 =

  let private getCorners (corners : char array) inp pos =
    let xDirs = [| NW; NE; SW; SE |]
    let rec loop i =
      i = 4 ||
        match Input.tryGet inp (Position.add pos xDirs[i]) with
        | Some c -> corners[i] <- c; loop (i + 1)
        | _ -> false
    loop 0

  let private xpell corners inp pos =
    match Input.tryGet inp pos with
    | Some 'A' ->
      if getCorners corners inp pos then
        match corners with
        | [| 'M'; 'M'; 'S'; 'S' |] -> 1
        | [| 'M'; 'S'; 'M'; 'S' |] -> 1
        | [| 'S'; 'M'; 'S'; 'M' |] -> 1
        | [| 'S'; 'S'; 'M'; 'M' |] -> 1
        | _ -> 0
      else 0
    | _ -> 0

  let solve (path : string) =
    let corners = Array.zeroCreate 4
    let inp = Input.fromFile path
    Array.sumBy (xpell corners inp) (Input.positions inp)