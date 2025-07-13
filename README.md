# Tetris

An F# implementation of Tetris game.

___

**Tetris.Engine** project contains core game logic. It can be used to create a Tetris game with your own UI.

_See [Script.fsx](Script.fsx) for usage example._

___

**Tetris.Console** is a TUI application allowing you to play it out of the box.

`dotnet run --project src\Tetris.Console`

___

**Todo:**

- add levels system;
- use Thuja for TUI implementation;
- implement wall kicks;
- extend scoring system;
- add ghost piece;
- implement Super Rotation System;
- add T-Spins pattern recognition;
- implement AI bot;
- ...
