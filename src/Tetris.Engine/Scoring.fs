module Tetris.Engine.Scoring

let init () =
  { Singles = 0
    Doubles = 0
    Triples = 0
    Tetrises = 0 }

let recognise (board : Board) (tetrimino : Tetrimino) =
  let completedLines =
    board.Field
    |> Map.toSeq
    |> Seq.map fst
    |> Seq.append tetrimino.Blocks
    |> Seq.groupBy (fun pos -> pos.Y)
    |> Seq.filter (fun (y, blocks) -> Seq.length blocks = board.Width)
    |> Seq.length

  let scoring = init()
  
  match completedLines with
  | 1 -> { scoring with Singles = 1 }
  | 2 -> { scoring with Doubles = 1 }
  | 3 -> { scoring with Triples = 1 }
  | 4 -> { scoring with Tetrises = 1 }
  | _ -> scoring

let append (a : Scoring) (b : Scoring) =
  { Singles = a.Singles + b.Singles
    Doubles = a.Doubles + b.Doubles
    Triples = a.Triples + b.Triples
    Tetrises = a.Tetrises + b.Tetrises }