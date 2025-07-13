[<AutoOpen>]
module Tetris.Engine.Types

type Shape =
  | O | I | T | L | J | S | Z

type Facing =
  | North | West | South | East

type Position = 
  { X: int; Y: int } 

type Board = 
  { Width: int
    Height: int
    Field: Map<Position, Shape> }

type Tetrimino =
  { Shape: Shape
    Facing: Facing
    Position: Position } 

type Tetrimino with
  member this.Blocks : Position list =
    let shape =
      match this.Shape with
      | O -> [ (-1, 0); (0,  0); (-1, 1); (0,  1) ]
      | I -> [ (-2, 0); (-1, 0); (0,  0); (1,  0) ]
      | T -> [ (-1, 0); (0,  0); (1,  0); (0,  1) ]
      | L -> [ (-1, 0); (0,  0); (1,  0); (-1, 1) ]
      | J -> [ (-1, 0); (0,  0); (1,  0); (1,  1) ]
      | S -> [ (0,  0); (1,  0); (-1, 1); (0,  1) ]
      | Z -> [ (-1, 0); (0,  0); (0,  1); (1,  1) ]

    // https://tetris.wiki/Original_Rotation_System
    let shape =
      if this.Shape = O then shape
      else 
        // https://en.wikipedia.org/wiki/Rotations_and_reflections_in_two_dimensions
        List.map (fun (x, y) ->
          match this.Facing with 
          | North -> (x, y)
          | West -> (y, -x)
          | South -> (-x, -y)
          | East ->  (-y, x)
        ) shape
        
    shape
    |> Seq.map (fun (x, y) -> { X = this.Position.X + x; Y = this.Position.Y + y })
    |> Seq.toList

type Command =
  | MoveLeft
  | MoveRight
  | MoveDown
  | RotateLeft
  | RotateRight
  | HardDrop

type Scoring  =
  { Singles: int
    Doubles: int
    Triples: int
    Tetrises: int }

type GameState =
  { Board: Board
    Current: Tetrimino
    Next: Tetrimino
    Scoring: Scoring
    IsGameOver: bool }
    