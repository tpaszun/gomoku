module Main where

import System.Environment

import Gomoku.Abstractions
import Gomoku.AI
import Gomoku.GameState
import Gomoku.InputParser

main :: IO ()
main = do
  args <- getArgs
  let firstArg = head args
  let player = case firstArg of
                 "b" -> Black
                 "w" -> White
                 _   -> error "invalid argument, allowed: 'b' or 'w'"
  gameState <- case player of
                 Black -> do
                   printMove $ Move 10 10 Black
                   return $ createGameState 19 [Move 10 10 Black]
                 White -> do
                   return $ createGameState 19 []
  loop gameState (otherPlayer player)
  return ()


loop gameState enemyPlayer = do
  enemyMove <- externalAgent enemyPlayer
  let gameStateAfterEnemysMove = updateGameState gameState enemyMove
  if gameIsOver $ evaluation gameStateAfterEnemysMove
    then do
      putStrLn "Lost"
      return ()
    else do
      let myMove = aiAgent gameStateAfterEnemysMove
      printMove myMove
      let gameStateAfterMyMove = updateGameState gameStateAfterEnemysMove myMove
      if gameIsOver $ evaluation gameStateAfterMyMove
        then do
          putStrLn "Won"
          return ()
        else
          loop gameStateAfterMyMove enemyPlayer

externalAgent :: Player -> IO Move
externalAgent player = do
  input <- getLine
  return $ parseMove player input

aiAgent :: GameState -> Move
aiAgent gameState = minimax (movesTreeOnlyBest 9) 4 gameState

printMove :: Move -> IO ()
printMove (Move x y _) = putStrLn $ "(" ++ show (x+1) ++ "," ++ show (y+1) ++ ")"
