module Tetris.Engine.Tests.BoardTests

open Expecto
open Expecto.Flip

open Tetris.Engine

let emptyBoard = Board.init (10, 20)

[<Tests>]
let tests = testList "Board" [
  testCase "LockDown should add tetrimino to field" <| fun _ ->
    let tetrimino = { Shape = O; Facing = North; Position = { X = 5; Y = 5 } }
    
    let newBoard = Board.lockDown tetrimino emptyBoard
    newBoard.Field.Count |> Expect.equal "Should add 4 blocks" 4
    newBoard.Field |> Map.containsKey { X = 5; Y = 5 } |> Expect.isTrue "Should contain block"

  testCase "Eliminate should clear completed lines" <| fun _ ->
    let completedLine = 
      [0..emptyBoard.Width-1] |> Seq.map (fun x -> { X = x; Y = emptyBoard.Height-1 }, O)
    
    let incompleteLine = 
      [{ X = 0; Y = emptyBoard.Height-2 }, O]
    
    let field = Seq.append completedLine incompleteLine |> Map.ofSeq

    let board = { emptyBoard with Field = field }
    
    let newBoard = Board.eliminate board
    newBoard.Field.Count |> Expect.equal "Should keep only incomplete blocks" 1
    newBoard.Field |> Map.containsKey { X = 0; Y = 19 } |> Expect.isTrue "Should shift block down"

  testCase "Eliminate should handle multiple lines" <| fun _ ->
    let lines = 
      [emptyBoard.Height-2..emptyBoard.Height-1] 
      |> Seq.collect (fun y -> Seq.map (fun x -> { X = x; Y = y }, O) [0..emptyBoard.Width-1])
      |> Map.ofSeq
    
    let board = { emptyBoard with Field = lines }
    let newBoard = Board.eliminate board
    newBoard.Field |> Map.isEmpty |> Expect.isTrue "Should clear all blocks"
]