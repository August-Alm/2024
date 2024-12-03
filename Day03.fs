module AdventOfCode.Day03

open System

[<Struct>]
type Input = { Start : int; Source : string } 
with
  static member ctor (s : string) = { Start = 0; Source = s }
  member this.Item with get i = this.Source[this.Start + i]
  member this.IsEmpty = this.Start = this.Source.Length
  member this.Prefix n =
    if this.Start + n < this.Source.Length then this.Source.AsSpan (this.Start, n)
    else ReadOnlySpan.Empty
  member this.IsPrefix (s : string) = this.Prefix(s.Length).SequenceEqual s

type Parser<'a> = Input -> ('a * Input) option

let inline retur (a : 'a) : Parser<'a> =
  fun cell -> Some (a, cell)

let inline (>>=) p ([<InlineIfLambda>]f) =
  p >> Option.bind (fun (a, c) -> (f a) c)

let (.>>.) p1 p2 = p1 >>= fun x1 -> p2 >>= fun x2 -> retur (x1, x2)

let (.>>) p1 p2 = p1 >>= fun x1 -> p2 >>= fun _ -> retur x1

let (>>.) p1 p2 = p1 >>= fun _ -> p2 >>= fun x2 -> retur x2

let inline map ([<InlineIfLambda>]f) p = p >>= (retur << f)

let (<*>) f p = map (fun (f, x) -> f x) (f .>>. p)

let (<|>) p1 p2 =
  fun inp ->
    match p1 inp with
    | Some r1 -> Some r1
    | None -> p2 inp

let pChar c : Parser<char> =
  fun inp ->
    if inp.IsEmpty then None
    elif inp[0] = c then
      Some (c, { inp with Start = inp.Start + 1 })
    else None

let pWord (w : string) : Parser<string> =
  fun inp ->
    if inp.IsEmpty then None
    elif inp.IsPrefix w then Some (w, { inp with Start = inp.Start + w.Length })
    else None

let pInt : Parser<int> =
  fun inp ->
    if inp.IsEmpty then None
    else
      let rec digits i = if Char.IsDigit inp[i] then digits (i + 1) else i
      match digits 0 with
      | 0 -> None
      | n ->
        match Int32.TryParse (inp.Prefix n) with
        | true, x -> Some (x, { inp with Start = inp.Start + n })
        | _ -> None


module Puzzle1 =

  open System.IO

  let pMul =
    pWord "mul" >>. pChar '(' >>. pInt .>> pChar ',' .>>. pInt .>> pChar ')'
    |> map (fun (x, y) -> x * y)
    
  let aggregate inp =
    let rec loop acc (inp : Input) =
      if inp.IsEmpty then acc
      else
        match pMul inp with
        | Some (x, inp) -> loop (acc + x) inp
        | None -> loop acc { inp with Start = inp.Start + 1 }
    loop 0 inp
  
  let solve (path : string) =
    File.ReadAllText path
    |> Input.ctor
    |> aggregate
