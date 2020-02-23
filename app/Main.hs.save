module Main where

import Control.Concurrent
import Control.Monad
import System.Console.ANSI
import System.IO

------------------------------------------------------------------------

data Player = Player Int Int
data Moves = Moves Int Int
data Scores = Scores Int Int
data World = World Player Player Moves Scores

data Command =
    Exit
  | MoveLeft1
  | MoveRight1
  | MoveUp1
  | MoveDown1
  | MoveLeft2
  | MoveRight2
  | MoveUp2
  | MoveDown2
  | Reset1
  | Reset2
  | Lessen
  | SwitchScores

getCommand :: IO Command
getCommand = do
  char <- getChar
  case char of
    'A' -> return MoveUp2
    'D' -> return MoveLeft2
    'B' -> return MoveDown2
    'C' -> return MoveRight2
    'Q' -> return Exit
    'w' -> return MoveUp1
    'a' -> return MoveLeft1
    's' -> return MoveDown1
    'd' -> return MoveRight1
    'o' -> return Reset1
    'O' -> return Reset2
    'q' -> return Lessen
    'R' -> return SwitchScores
    _ -> getCommand

updateWorld :: World -> Command -> World
updateWorld (World (Player x1 y1) (Player x2 y2) (Moves x01 x02) (Scores sh ss)) command =
  case command of
    MoveUp1    | x1 >  1 && x01 < 400  -> World (Player (x1 - 1) y1) (Player x2 y2) (Moves (x01 + 1) x02) (Scores sh ss)
    MoveDown1  | x1 < 40 && x01 < 400  -> World (Player (x1 + 1) y1) (Player x2 y2) (Moves (x01 + 1) x02) (Scores sh ss)
    MoveLeft1  | y1 >  1 && x01 < 400  -> World (Player x1 (y1 - 1)) (Player x2 y2) (Moves (x01 + 1) x02) (Scores sh ss)
    MoveRight1 | y1 < 148 && x01 < 400 -> World (Player x1 (y1 + 1)) (Player x2 y2) (Moves (x01 + 1) x02) (Scores sh ss)
    MoveUp2    | x2 >  1 && x02 < 600  -> World (Player x1 y1) (Player (x2 - 1) y2) (Moves x01 (x02 + 1)) (Scores sh ss)
    MoveDown2  | x2 < 40 && x02 < 600  -> World (Player x1 y1) (Player (x2 + 1) y2) (Moves x01 (x02 + 1)) (Scores sh ss)
    MoveLeft2  | y2 >  1 && x02 < 600  -> World (Player x1 y1) (Player x2 (y2 - 1)) (Moves x01 (x02 + 1)) (Scores sh ss)
    MoveRight2 | y2 < 148 && x02 < 600 -> World (Player x1 y1) (Player x2 (y2 + 1)) (Moves x01 (x02 + 1)) (Scores sh ss)
    Lessen     | x01 >= 3              -> World (Player x1 y1) (Player x2 y2) (Moves (x01 - 3) x02) (Scores sh ss)
    Lessen     | x01 == 2              -> World (Player x1 y1) (Player x2 y2) (Moves (x01 - 2) x02) (Scores sh ss)
    Lessen     | x01 == 1              -> World (Player x1 y1) (Player x2 y2) (Moves (x01 - 1) x02) (Scores sh ss)
    Reset1                             -> World (Player 2 2) (Player 2 147) (Moves 0 0) (Scores sh ss)
    Reset2                             -> World (Player 2 2) (Player 2 147) (Moves 0 0) (Scores 0 0)
    SwitchScores                       -> World (Player x1 y1) (Player x2 y2) (Moves x01 x02) (Scores ss sh)
    _ -> World (Player x1 y1) (Player x2 y2) (Moves x01 x02) (Scores sh ss)

mainLoop :: World -> IO ()
mainLoop world = do
  drawWorld world
  command <- getCommand
  case command of
    Exit -> handleExit
    _ -> do
      let world'  = updateWorld world command
      world'' <- resetWorldIf world world'
      mainLoop world''

resetWorldIf :: World -> World -> IO World
resetWorldIf
    (World (Player x1 y1) (Player x2 y2) (Moves x01 x02) (Scores sh ss))
    world2@(World (Player x1' y1') (Player x2' y2') (Moves x01' x02') (Scores sh' ss')) =
  if x1' == x2' && y1' == y2' || x02 == 600
    then do
      drawWorld world2
      waitForKeyPress
      if x02 == 600
        then return (World (Player 2 2) (Player 2 147) (Moves 0 0) (Scores (sh + 1) ss))
        else return (World (Player 2 2) (Player 2 147) (Moves 0 0) (Scores sh (ss + 1)))
    else
      return world2
    where
      waitForKeyPress = do
        c <- getChar
        unless (c == ' ') waitForKeyPress

main :: IO ()
main = do
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  hideCursor
  setTitle "Prey vs Hunter"
  clearScreen
  mainLoop (World (Player 2 2) (Player 2 147) (Moves 0 0) (Scores 0 0))

drawWorld :: World -> IO ()
drawWorld (World (Player x1 y1) (Player x2 y2) (Moves x01 x02) (Scores sh ss)) = do
  clearScreen
  setCursorPosition 0 0
  putStr ("Prey: " ++ (show x01) ++ "/400 Predator: " ++ (show x02) ++ "/600")
  setCursorPosition 0 35
  putStr ("Prey Score: " ++ (show sh) ++ "  Predator Score: " ++ (show ss))
  setCursorPosition x1 y1
  putStr "o"
  setCursorPosition x2 y2
  putStr "C"
  if x1 == x2 && y1 == y2
    then do
      clearScreen
      setCursorPosition 19 74
      putStr "The Predator Feasts"
      setCursorPosition 21 74
      putStr "Press Space To Continue..."
    else return ()
  if x02 == 600
    then do
      clearScreen
      setCursorPosition 19 74
      putStr "The Prey Escapes"
      setCursorPosition 21 74
      putStrLn "Press Space To Continue..."
    else return ()

handleExit :: IO ()
handleExit = do
  clearScreen
  setCursorPosition 20 74
  showCursor
  setSGR [Reset]
