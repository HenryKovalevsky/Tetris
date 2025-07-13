#I @"src\Tetris.Engine"

#load @"Types.fs"
#load @"Tetrimino.fs"
#load @"Board.fs"
#load @"Scoring.fs"
#load @"GameState.fs"

#I @"tests\Tetris.Engine.Tests"

#load @"Helpers.fs"

open System

open Tetris.Engine
open Tetris.Engine.Tests.Helpers

// setup
let boardSize = 10, 20
let randomizer = Random 42 |> _.NextDouble

let init () = GameState.initWith randomizer boardSize
let update command state = GameState.updateWith randomizer command state

// program
let mutable state = init ()

Tetrimino.toArray2D boardSize state.Current |> printfn "%A"

state <- update MoveDown state
state <- update MoveLeft state
state <- update MoveRight state
state <- update RotateLeft state
state <- update RotateRight state
state <- update HardDrop state

Board.toArray2D state.Board |> printfn "%A"
Tetrimino.toArray2D boardSize state.Current |> printfn "%A"