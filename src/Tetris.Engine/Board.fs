module Tetris.Engine.Board 

let init (width, height) =
  { Width = width
    Height = height
    Field = Map.empty }

let lockDown (tetrimino : Tetrimino) (board : Board) =
  let newField = 
    List.fold (fun field pos ->
      Map.add pos tetrimino.Shape field
    ) board.Field tetrimino.Blocks
    
  { board with Field = newField }

let eliminate (board : Board) =
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
  
  { board with Field = newField }

  
