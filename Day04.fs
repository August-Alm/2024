module AdventOfCode.Day04

open System.IO

[<Struct>]
type Position = { Col : int; Row : int }

type Input = char array array

[<RequireQualifiedAccess>]
module Input =

  let fromFile (path : string) =
    File.ReadAllLines path
    |> Array.map (fun s -> s.ToCharArray ())
  
  let isWithin (inp : Input) (pos : Position) =
    (pos.Row >= 0 && pos.Row < inp.Length) &&
    (pos.Col >= 0 && pos.Col < inp[pos.Row].Length)
  
  let inline get (inp : Input) (pos : Position) =
    inp[pos.Row][pos.Col] 
  
  let private directions =
    [|
      { Col = -1; Row = -1 }
      { Col =  0; Row = -1 }
      { Col =  1; Row = -1 }
      { Col = -1; Row =  0 }
      { Col =  1; Row =  0 }
      { Col = -1; Row =  1 }
      { Col =  0; Row =  1 }
      { Col =  1; Row =  1 }
    |]
  
  let neighbors (inp : Input) (pos : Position) =
    directions |> Array. choose (fun dir ->
      let nb = { Col = pos.Col + dir.Col; Row = pos.Row + dir.Row }
      if isWithin inp nb then Some nb else None)
  
  let positions (inp : Input) =
    let result = ResizeArray ()
    inp |> Array.iteri (fun i row ->
      row |> Array.iteri (fun j _ ->
        result.Add { Row = i; Col = j }))
    Array.ofSeq result
  

module Puzzle1 =

  let rec private spell letter inp pos : int =
    let inline go letter inp pos =
      Input.neighbors inp pos
      |> Array.sumBy (fun nb -> spell letter inp nb)
    match letter with
    | 'X' when Input.get inp pos = 'X' -> go 'M' inp pos
    | 'M' when Input.get inp pos = 'M' -> go 'A' inp pos
    | 'A' when Input.get inp pos = 'A' -> go 'S' inp pos
    | 'S' when Input.get inp pos = 'S' -> 1
    | _ -> 0
  
  let solve (path : string) =
    let inp = Input.fromFile path
    Input.positions inp
    |> Array.sumBy (spell 'X' inp)



  


