module Tetris.Engine.Tests.TypesTests

open Expecto
open Expecto.Flip

open Tetris.Engine
open Helpers

[<Tests>]
let tests = testList "Shape Visualization" [
  testCase "O shape should be correct in all facings" <| fun _ ->
    let expected = 
        [[' '; ' '; ' '; ' ']
         [' '; 'O'; 'O'; ' ']
         [' '; 'O'; 'O'; ' ']
         [' '; ' '; ' '; ' ']]

    [ North; West; South; East ]
    |> List.iter (fun facing ->
        let center = { X = 2; Y = 1 }
        let tetrimino = { Shape = O; Facing = facing; Position = center }
        tetrimino 
        |> Tetrimino.toArray2D (4, 4)
        |> Expect.equal $"Facing {facing} should be correct" (array2D expected))

  testCase "I-shape rotation should be correct" <| fun _ ->
    [ North,
      [[' '; ' '; ' '; ' '; ' ']
       [' '; ' '; ' '; ' '; ' ']
       ['I'; 'I'; 'I'; 'I'; ' ']
       [' '; ' '; ' '; ' '; ' ']
       [' '; ' '; ' '; ' '; ' ']]

      West,
      [[' '; ' '; ' '; ' '; ' ']
       [' '; ' '; 'I'; ' '; ' ']
       [' '; ' '; 'I'; ' '; ' ']
       [' '; ' '; 'I'; ' '; ' ']
       [' '; ' '; 'I'; ' '; ' ']] 

      South,
      [[' '; ' '; ' '; ' '; ' ']
       [' '; ' '; ' '; ' '; ' ']
       [' '; 'I'; 'I'; 'I'; 'I']
       [' '; ' '; ' '; ' '; ' ']
       [' '; ' '; ' '; ' '; ' ']]

      East,
      [[' '; ' '; 'I'; ' '; ' ']
       [' '; ' '; 'I'; ' '; ' ']
       [' '; ' '; 'I'; ' '; ' ']
       [' '; ' '; 'I'; ' '; ' ']
       [' '; ' '; ' '; ' '; ' ']] ]
    |> List.iter (fun (facing, expected) ->
        let center = { X = 2; Y = 2 }
        let tetrimino = { Shape = I; Facing = facing; Position = center }
        tetrimino 
        |> Tetrimino.toArray2D (5, 5)
        |> Expect.equal $"Facing {facing} should be correct" (array2D expected))

  testCase "T-shape rotation should be correct" <| fun _ ->
    [ North,
      [[' '; ' '; ' ']
       ['T'; 'T'; 'T']
       [' '; 'T'; ' ']]

      West,
      [[' '; 'T'; ' ']
       [' '; 'T'; 'T']
       [' '; 'T'; ' ']]

      South,
      [[' '; 'T'; ' ']
       ['T'; 'T'; 'T']
       [' '; ' '; ' ']]

      East,
      [[' '; 'T'; ' ']
       ['T'; 'T'; ' ']
       [' '; 'T'; ' ']] ]
      |> List.iter (fun (facing, expected) ->
          let center = { X = 1; Y = 1 }
          let tetrimino = { Shape = T; Facing = facing; Position = center }
          tetrimino 
          |> Tetrimino.toArray2D (3, 3)
          |> Expect.equal $"Facing {facing} should be correct" (array2D expected))

  testCase "L-shape rotation should be correct" <| fun _ ->
    [ North,
      [[' '; ' '; ' ']
       ['L'; 'L'; 'L']
       ['L'; ' '; ' ']]

      West,
      [[' '; 'L'; ' ']
       [' '; 'L'; ' ']
       [' '; 'L'; 'L']]

      South,
      [[' '; ' '; 'L']
       ['L'; 'L'; 'L']
       [' '; ' '; ' ']]

      East,
      [['L'; 'L'; ' ']
       [' '; 'L'; ' ']
       [' '; 'L'; ' ']] ]
    |> List.iter (fun (facing, expected) ->
        let center = { X = 1; Y = 1 }
        let tetrimino = { Shape = L; Facing = facing; Position = center }
        tetrimino 
        |> Tetrimino.toArray2D (3, 3)
        |> Expect.equal $"Facing {facing} should be correct" (array2D expected))

  testCase "J-shape rotation should be correct" <| fun _ ->
    [ North,
      [[' '; ' '; ' ']
       ['J'; 'J'; 'J']
       [' '; ' '; 'J']]

      West,
      [[' '; 'J'; 'J']
       [' '; 'J'; ' ']
       [' '; 'J'; ' ']]

      South,
      [['J'; ' '; ' ']
       ['J'; 'J'; 'J']
       [' '; ' '; ' ']]

      East,
      [[' '; 'J'; ' ']
       [' '; 'J'; ' ']
       ['J'; 'J'; ' ']] ]
    |> List.iter (fun (facing, expected) ->
        let center = { X = 1; Y = 1 }
        let tetrimino = { Shape = J; Facing = facing; Position = center }
        tetrimino 
        |> Tetrimino.toArray2D (3, 3)
        |> Expect.equal $"Facing {facing} should be correct" (array2D expected))

  testCase "S-shape rotation should be correct" <| fun _ ->
    [ North,
      [[' '; ' '; ' ']
       [' '; 'S'; 'S']
       ['S'; 'S'; ' ']]

      West,
      [[' '; 'S'; ' ']
       [' '; 'S'; 'S']
       [' '; ' '; 'S']]

      South,
      [[' '; 'S'; 'S']
       ['S'; 'S'; ' ']
       [' '; ' '; ' ']]

      East,
      [['S'; ' '; ' ']
       ['S'; 'S'; ' ']
       [' '; 'S'; ' ']] ]
    |> List.iter (fun (facing, expected) ->
        let center = { X = 1; Y = 1 }
        let tetrimino = { Shape = S; Facing = facing; Position = center }
        tetrimino 
        |> Tetrimino.toArray2D (3, 3)
        |> Expect.equal $"Facing {facing} should be correct" (array2D expected))

  testCase "Z-shape rotation should be correct" <| fun _ ->
    [ North,
      [[' '; ' '; ' ']
       ['Z'; 'Z'; ' ']
       [' '; 'Z'; 'Z']]

      West,
      [[' '; ' '; 'Z']
       [' '; 'Z'; 'Z']
       [' '; 'Z'; ' ']]

      South,
      [['Z'; 'Z'; ' ']
       [' '; 'Z'; 'Z']
       [' '; ' '; ' ']]

      East,
      [[' '; 'Z'; ' ']
       ['Z'; 'Z'; ' ']
       ['Z'; ' '; ' ']] ]
    |> List.iter (fun (facing, expected) ->
        let center = { X = 1; Y = 1 }
        let tetrimino = { Shape = Z; Facing = facing; Position = center }
        tetrimino 
        |> Tetrimino.toArray2D (3, 3)
        |> Expect.equal $"Facing {facing} should be correct" (array2D expected))
]