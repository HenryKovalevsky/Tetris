module Tetris.Engine.Tests.GameStateTests

open Expecto
open Expecto.Flip

open Tetris.Engine

let testRandomizer = System.Random 42 |>_.NextDouble

let emptyBoard = Board.init (10, 20)
let initialState = GameState.initWith testRandomizer (10, 20)

[<Tests>]
let tests = testList "GameState" [
  testCase "Game should end when new tetrimino collides" <| fun _ ->
    let field = 
      [ for y in 0..emptyBoard.Height-1 do 
        for x in 0..emptyBoard.Width-2 do // almost completed line to prevent elimination
        yield { X = x; Y = y }, O ] 
      |> Map.ofList
    
    let board = { emptyBoard with Field = field }
    let state = { initialState with Board = board }
    
    let newState = GameState.updateWith testRandomizer HardDrop state
    newState.IsGameOver |> Expect.isTrue "Game should be over"

  testCase "HardDrop should lock current piece and spawn new" <| fun _ ->
    let newState = GameState.updateWith testRandomizer HardDrop initialState
    
    newState.Board.Field.Count |> Expect.equal "Should lock 4 blocks" 4
    newState.Current |> Expect.equal "Should spawn new tetrimino" initialState.Next

  testCase "Multiple commands should process correctly" <| fun _ ->
    let newState = 
      initialState
      |> GameState.updateWith testRandomizer MoveDown
      |> GameState.updateWith testRandomizer MoveRight
      |> GameState.updateWith testRandomizer MoveLeft
      |> GameState.updateWith testRandomizer RotateRight
      |> GameState.updateWith testRandomizer RotateLeft
      |> GameState.updateWith testRandomizer HardDrop
    
    newState.Board.Field.Count |> Expect.equal "Should have locked blocks" 4
]