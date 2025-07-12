#I @"src\Tetris.Engine"

#load @"Types.fs"
#load @"Tetrimino.fs"
#load @"Board.fs"
#load @"Scoring.fs"
#load @"GameState.fs"

open System

open Tetris.Engine

// helpers
let boardToArray2D (board : Board) =
  let array = Array2D.create board.Height board.Width ' '

  board.Field
  |> Map.toSeq
  |> Seq.iter (fun (pos, shape) -> Array2D.set array pos.Y pos.X (shape.ToString().[0]))

  array

let tetriminoToArray2D boardSize (tetrimino : Tetrimino) =
  let width, height = boardSize
  let array = Array2D.create height width ' '

  tetrimino.Blocks
  |> Seq.iter (fun pos -> Array2D.set array pos.Y pos.X (tetrimino.Shape.ToString().[0]))

  array

// setup
let boardSize = 10, 20
let randomizer = Random 42 |> _.NextDouble

let init () = GameState.initWith randomizer boardSize
let update command state = GameState.updateWith randomizer command state

// program
let mutable state = init ()

tetriminoToArray2D boardSize state.Current |> printfn "%A"

state <- update MoveDown state
state <- update MoveLeft state
state <- update MoveRight state
state <- update RotateLeft state
state <- update RotateRight state
state <- update HardDrop state

boardToArray2D state.Board |> printfn "%A"
tetriminoToArray2D boardSize state.Current |> printfn "%A"