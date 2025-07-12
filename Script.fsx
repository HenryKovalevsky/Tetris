// domain
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

type Command =
  | MoveLeft
  | MoveRight
  | MoveDown
  | Rotate
  | HardDrop

type GameState =
  { Board: Board
    Current: Tetrimino
    Next: Tetrimino
    Score: int
    IsGameOver: bool }
    
type Tetrimino with
  member this.Blocks : Position list =
    let shape =
      match this.Shape with
      | O -> [ (-1, 0); (0,  0); (-1, 1); (0,  1) ]
      | I -> [ (-2, 0); (-1, 0); (0,  0); (1,  0) ]
      | T -> [ (-1, 0); (0,  0); (1,  0); (0,  1) ]
      | L -> [ (-1, 0); (0,  0); (1,  0); (1,  1) ]
      | J -> [ (-1, 0); (0,  0); (1,  0); (-1, 1) ]
      | S -> [ (-1, 0); (0,  0); (0,  1); (1,  1) ]
      | Z -> [ (0,  0); (1,  0); (-1, 1); (0,  1) ]

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

// game logic
let getRandomTetrimino board =
    let shapes = [ O; I; T; L; J; S; Z ]
    let shape = Seq.randomChoice shapes

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

let rotate (tetrimino : Tetrimino) =
  let newFacing =
    match tetrimino.Facing with
    | North -> East
    | East -> South
    | South -> West
    | West -> North

  { tetrimino with Facing = newFacing }

let hardDrop (board : Board) (tetrimino : Tetrimino) =
  let rec dropLoop tetrimino =
    let movedDown = { tetrimino with Tetrimino.Position.Y = tetrimino.Position.Y + 1 }
    if isValidPosition board movedDown then
      dropLoop movedDown
    else
      tetrimino
  
  dropLoop tetrimino

let lockDown (board : Board) (tetrimino : Tetrimino) =
  let newField = 
    List.fold (fun field pos ->
      Map.add pos tetrimino.Shape field
    ) board.Field tetrimino.Blocks
    
  { board with Field = newField }

let clearLines (board : Board) =
  let completedLines =
    board.Field
    |> Map.toSeq
    |> Seq.groupBy (fun (pos, _) -> pos.Y)
    |> Seq.filter (fun (_, cells) -> Seq.length cells = board.Width)
    |> Seq.map fst
    |> Set.ofSeq

  let newField =
    board.Field
    |> Map.toSeq
    |> Seq.filter (fun (pos, _) -> not <| Set.contains pos.Y completedLines)
    |> Seq.map (fun (pos, cell) ->
        let linesBelow = 
          completedLines 
          |> Set.filter (fun y -> y > pos.Y) 
          |> Set.count
          
        { pos with Y = pos.Y + linesBelow }, cell)
    |> Map.ofSeq

  { board with Field = newField }, completedLines.Count

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

let initGameState () =
  let board = 
    { Width = 10
      Height = 20
      Field = Map.empty }

  let current = getRandomTetrimino board
  let next = getRandomTetrimino board

  { Board = board
    Current = current
    Next = next
    Score = 0
    IsGameOver = false }

let updateGameState command state =
  if state.IsGameOver then state
  else
    let moved = 
      match command with
      | MoveLeft -> { state.Current with Tetrimino.Position.X = state.Current.Position.X - 1 }
      | MoveRight -> { state.Current with Tetrimino.Position.X = state.Current.Position.X + 1 }
      | MoveDown -> { state.Current with Tetrimino.Position.Y = state.Current.Position.Y + 1 }
      | Rotate -> rotate state.Current
      | HardDrop -> hardDrop state.Board state.Current
    
    if isValidPosition state.Board moved then
      { state with Current = moved }
    else 
      if command = MoveDown then
        let newBoard = lockDown state.Board state.Current
        let clearedBoard, clearedLines = clearLines newBoard
        let next = getRandomTetrimino clearedBoard
        let isGameOver = not <| isValidPosition clearedBoard next
        
        { Board = clearedBoard
          Current = state.Next
          Next = next
          Score = clearedLines
          IsGameOver = isGameOver }
      else state

// program
let mutable state = initGameState()

tetriminoToArray2D (10, 20) state.Current |> printfn "%A"

state <- updateGameState MoveLeft state
state <- updateGameState MoveRight state
state <- updateGameState MoveDown state
state <- updateGameState Rotate state
state <- updateGameState HardDrop state

boardToArray2D state.Board |> printfn "%A"
tetriminoToArray2D (10, 20) state.Current |> printfn "%A"