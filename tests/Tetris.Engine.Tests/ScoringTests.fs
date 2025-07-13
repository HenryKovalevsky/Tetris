module Tetris.Engine.Tests.ScoringTests

open Expecto
open Expecto.Flip

open Tetris.Engine

let emptyBoard = Board.init (10, 20)
let initialScoring = Scoring.init ()
let tetrimino = { Shape = I; Facing = West; Position = { X = 0; Y = 17 } } // vertical I in the bottom left corner

[<Tests>]
let tests = testList "Scoring" [
  testCase "Scoring should recognise completed single" <| fun _ ->
    let field = 
      [emptyBoard.Height-1] 
      |> Seq.collect (fun y -> Seq.map (fun x -> { X = x; Y = y }, O)  [1..emptyBoard.Width-1])
      |> Map.ofSeq
    
    let board = { emptyBoard with Field = field }
    let newScoring = Scoring.recognise board tetrimino

    newScoring |> Expect.equal "Should recognise single" { initialScoring with Singles = 1}

  testCase "Scoring should recognise completed double" <| fun _ ->
    let field = 
      [emptyBoard.Height-2..emptyBoard.Height-1] 
      |> Seq.collect (fun y -> Seq.map (fun x -> { X = x; Y = y }, O) [1..emptyBoard.Width-1])
      |> Map.ofSeq
    
    let board = { emptyBoard with Field = field }
    let newScoring = Scoring.recognise board tetrimino

    newScoring |> Expect.equal "Should recognise double" { initialScoring with Doubles = 1}

  testCase "Scoring should recognise completed triple" <| fun _ ->
    let field = 
      [emptyBoard.Height-3..emptyBoard.Height-1] 
      |> Seq.collect (fun y -> Seq.map (fun x -> { X = x; Y = y }, O)  [1..emptyBoard.Width-1])
      |> Map.ofSeq
    
    let board = { emptyBoard with Field = field }
    let newScoring = Scoring.recognise board tetrimino

    newScoring |> Expect.equal "Should recognise triple" { initialScoring with Triples = 1}

  testCase "Scoring should recognise completed tetris" <| fun _ ->
    let field = 
      [emptyBoard.Height-4..emptyBoard.Height-1] 
      |> Seq.collect (fun y -> Seq.map (fun x -> { X = x; Y = y }, O)  [1..emptyBoard.Width-1])
      |> Map.ofSeq
    
    let board = { emptyBoard with Field = field }
    let newScoring = Scoring.recognise board tetrimino

    newScoring |> Expect.equal "Should recognise tetris" { initialScoring with Tetrises = 1}
]