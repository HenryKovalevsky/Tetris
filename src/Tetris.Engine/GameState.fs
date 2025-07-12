module Tetris.Engine.GameState

let initWith randomizer boardSize =
  let board = Board.init boardSize
  let scoring = Scoring.init()
  let current = Tetrimino.generateWith randomizer board
  let next = Tetrimino.generateWith randomizer board

  { Board = board
    Current = current
    Next = next
    Scoring = scoring
    IsGameOver = false }

let updateWith randomizer command state =
  if state.IsGameOver then state
  else
    let moved = 
      match command with
      | MoveLeft -> Tetrimino.moveLeft state.Current
      | MoveRight -> Tetrimino.moveRight state.Current
      | MoveDown -> Tetrimino.moveDown state.Current
      | RotateLeft -> Tetrimino.rotateLeft state.Current
      | RotateRight -> Tetrimino.rotateRight state.Current
      | HardDrop -> Tetrimino.hardDrop state.Board state.Current
    
    let isValidMove = Tetrimino.isValidPosition state.Board moved
    let needToLock = command = HardDrop || command = MoveDown && not isValidMove
      
    let state = 
      if isValidMove then
        { state with Current = moved }
      else state

    if needToLock then
      let scoring =
        Scoring.recognise state.Board state.Current
        |> Scoring.append state.Scoring

      let newBoard = 
        state.Board
        |> Board.lockDown state.Current
        |> Board.eliminate

      let next = Tetrimino.generateWith randomizer state.Board
      let isGameOver = not <| Tetrimino.isValidPosition newBoard next
      
      { Board = newBoard
        Current = state.Next
        Next = next
        Scoring = scoring
        IsGameOver = isGameOver }
    else state