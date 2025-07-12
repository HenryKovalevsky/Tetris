module Tetris.Engine.Tetrimino

let generateWith randomizer (board : Board) =
  let shapes = [ O; I; T; L; J; S; Z ]
  let shape = Seq.randomChoiceBy randomizer shapes

  let startFacing = North
  let startPosition = { X = board.Width / 2 + 1; Y = 0 }

  { Shape = shape
    Facing = startFacing
    Position = startPosition }

let isValidPosition (board : Board) (tetrimino : Tetrimino) =
  tetrimino.Blocks
  |> List.forall (fun pos ->
      let isInBoard = 
        pos.X >= 0 && pos.X < board.Width && 
        pos.Y >= 0 && pos.Y < board.Height

      let isEmptyCell = 
        not <| Map.containsKey pos board.Field

      isInBoard && isEmptyCell)

let moveLeft (tetrimino : Tetrimino) =
  { tetrimino with Tetrimino.Position.X = tetrimino.Position.X - 1 }

let moveRight (tetrimino : Tetrimino) =
  { tetrimino with Tetrimino.Position.X = tetrimino.Position.X + 1 }

let moveDown (tetrimino : Tetrimino) =
  { tetrimino with Tetrimino.Position.Y = tetrimino.Position.Y + 1 }

let rotateLeft (tetrimino : Tetrimino) =
  let newFacing =
    match tetrimino.Facing with
    | North -> West
    | West -> South
    | South -> East
    | East -> North

  { tetrimino with Facing = newFacing }

let rotateRight (tetrimino : Tetrimino) =
  let newFacing =
    match tetrimino.Facing with
    | North -> East
    | East -> South
    | South -> West
    | West -> North

  { tetrimino with Facing = newFacing }

let hardDrop (board : Board) (tetrimino : Tetrimino) =
  let rec dropLoop tetrimino =
    let movedDown = moveDown tetrimino
    if isValidPosition board movedDown then
      dropLoop movedDown
    else
      tetrimino
  
  dropLoop tetrimino