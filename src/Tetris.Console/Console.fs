module Tetris.Console 

open System
open System.Threading

open Tetris.Engine

module TUI =
  let private getColor = function
    | O -> ConsoleColor.Yellow
    | I -> ConsoleColor.Cyan
    | T -> ConsoleColor.DarkMagenta
    | L -> ConsoleColor.DarkYellow
    | J -> ConsoleColor.DarkBlue
    | S -> ConsoleColor.Green
    | Z -> ConsoleColor.Red

  let drawBorders (board : Board) =
    Console.Clear()

    Console.ForegroundColor <- ConsoleColor.White
    let border = String('═', board.Width * 2)
    
    Console.SetCursorPosition(0, 0)
    Console.WriteLine ("╔" + border + "╗")

    for y in 0..board.Height-1 do
      Console.SetCursorPosition(0, y + 1)
      Console.Write "║"
      Console.SetCursorPosition(board.Width * 2 + 1, y + 1)
      Console.Write "║"
    
    Console.SetCursorPosition(0, board.Height + 1)
    Console.WriteLine ("╚" + border + "╝")

  let drawBoard (board : Board) (current : Tetrimino) =
    let blocks = 
      current.Blocks 
      |> List.map (fun p -> p, current.Shape)
      |> Seq.append (Map.toSeq board.Field)
      |> Seq.toList
    
    for y in 0..board.Height-1 do
      Console.SetCursorPosition(1, y + 1)
      
      for x in 0..board.Width-1 do
        let pos = { X = x; Y = y }
        match Seq.tryFind (fun (p, _) -> p = pos) blocks with
        | Some (_, tetrimino) -> 
            Console.ForegroundColor <- getColor tetrimino
            Console.Write "██"
        | None -> 
            Console.Write "  "

      Console.ForegroundColor <- ConsoleColor.White

  let drawInfo (board : Board) (next : Tetrimino) (score : int) =
    let margin = board.Width * 2 + 10

    Console.ForegroundColor <- ConsoleColor.White
    Console.SetCursorPosition(margin, 1)
    Console.WriteLine "Next:"
    
    let normalized = 
      next.Blocks 
      |> Seq.map (fun p -> p.X - next.Position.X, p.Y - next.Position.Y) 
      |> Set.ofSeq
    
    for y in -2..2 do
      Console.SetCursorPosition(margin, 4 + y)
      for x in -2..2 do
        if Set.contains (x, y) normalized then
          Console.ForegroundColor <- getColor next.Shape
          Console.Write "██"
        else
          Console.Write "  "

    Console.ForegroundColor <- ConsoleColor.White
    Console.SetCursorPosition(margin, 8)
    Console.WriteLine $"Score: {score}"
    Console.SetCursorPosition(margin, 10)
    Console.WriteLine "Controls:"
    Console.SetCursorPosition(margin, 11)
    Console.WriteLine "← → : Move"
    Console.SetCursorPosition(margin, 12)
    Console.WriteLine "↑   : Rotate"
    Console.SetCursorPosition(margin, 13)
    Console.WriteLine "↓   : Soft drop"
    Console.SetCursorPosition(margin, 14)
    Console.WriteLine "Space : Hard drop"
    Console.SetCursorPosition(margin, 15)
    Console.WriteLine "Ctrl+C : Exit"

  let drawGameOver state =
    Console.SetCursorPosition(state.Board.Width / 2, state.Board.Height / 2)
    Console.ForegroundColor <- ConsoleColor.Red
    Console.WriteLine "GAME OVER!"

module Game =
  let private getScore (scoring : Scoring) =
    scoring.Singles * 100 + 
    scoring.Doubles * 300 + 
    scoring.Triples * 500 + 
    scoring.Tetrises * 1500

  let private render state =
    TUI.drawBoard state.Board state.Current
    TUI.drawInfo state.Board state.Next (getScore state.Scoring)

    if state.IsGameOver then
      TUI.drawGameOver state

  let private handleInput () =
    if Console.KeyAvailable then
      match Console.ReadKey(true).Key with
      | ConsoleKey.LeftArrow -> Some MoveLeft
      | ConsoleKey.RightArrow -> Some MoveRight
      | ConsoleKey.DownArrow -> Some MoveDown
      | ConsoleKey.UpArrow -> Some RotateRight
      | ConsoleKey.Spacebar -> Some HardDrop
      | _ -> None
    else None

  let private exit () =
    Console.ForegroundColor <- ConsoleColor.White
    Console.SetCursorPosition(0, 23)
    Console.CursorVisible <- true
    exit 0

  let start () =
    Console.CursorVisible <- false
    Console.Title <- "F# Tetris"
    Console.OutputEncoding <- Text.Encoding.UTF8   
    Console.CancelKeyPress.Add(fun args -> 
      args.Cancel <- true
      exit()
    )
    
    // setup
    let speed = 500 
    let boardSize = 10, 20
    let randomizer = Random () |> _.NextDouble

    let init () = GameState.initWith randomizer boardSize
    let update command state = GameState.updateWith randomizer command state

    let mutable state = init()
    let mutable lastTick = DateTime.Now

    TUI.drawBorders state.Board

    // main loop
    while not state.IsGameOver do
      match handleInput() with
      | Some command ->
          state <- update command state
      | None ->
          let now = DateTime.Now
          let delta =  now - lastTick
          if delta.TotalMilliseconds > float speed then
            state <- update MoveDown state
            lastTick <- now
          else
            Thread.Sleep 16 // ~60 FPS
      
      render state

    exit()