module Tetris.Engine.Tests.Helpers

open Tetris.Engine

module Tetrimino =
  type Shape with
    member this.ToChar() = 
      match this with
      | O -> 'O' | I -> 'I' | T -> 'T' | L -> 'L' | J -> 'J' | S -> 'S' | Z -> 'Z'

  let toArray2D boardSize (tetrimino : Tetrimino) =
    let width, height = boardSize
    let array = Array2D.create height width ' '

    tetrimino.Blocks
    |> Seq.iter (fun pos -> Array2D.set array pos.Y pos.X (tetrimino.Shape.ToChar()))
    
    array

module Board =
  open Tetrimino
  
  let toArray2D (board : Board) =
    let array = Array2D.create board.Height board.Width ' '

    board.Field
    |> Map.toSeq
    |> Seq.iter (fun (pos, shape) -> Array2D.set array pos.Y pos.X (shape.ToChar()))

    array