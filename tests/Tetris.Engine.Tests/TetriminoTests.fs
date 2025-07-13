module Tetris.Engine.Tests.TetriminoTests

open Expecto
open Expecto.Flip

open Tetris.Engine

let emptyBoard = Board.init (10, 20)

[<Tests>]
let tests = testList "Tetrimino" [
  testCase "isValidPosition should detect collisions" <| fun _ ->
    let board = 
      { emptyBoard with Field = Map.ofList [ { X = 5; Y = 5 }, O ] }
    
    let tetrimino = { Shape = O; Facing = North; Position = { X = 5; Y = 5 } }
    Tetrimino.isValidPosition board tetrimino 
    |> Expect.isFalse "Collision should be detected"

  testCase "Hard drop should reach bottom" <| fun _ ->
    let tetrimino = { Shape = O; Facing = North; Position = { X = 5; Y = 0 } }
    let dropped = Tetrimino.hardDrop emptyBoard tetrimino
    dropped.Position.Y |> Expect.equal "Should be at bottom" (emptyBoard.Height - 2)

  testCase "Move should prevent out-of-bounds" <| fun _ ->
    [ Tetrimino.moveLeft, { X = 0; Y = 0 }
      Tetrimino.moveRight, { X = emptyBoard.Width - 1; Y = 0 }]
    |> List.iter (fun (move, position) ->
        let tetrimino = { Shape = O; Facing = North; Position = position }
        move tetrimino
        |> Tetrimino.isValidPosition emptyBoard
        |> Expect.isFalse $"Should prevent moving out of bounds")
]