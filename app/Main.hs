module Main where

import Gomoku.Abstractions
import Gomoku.BitBoardImpl
import Gomoku.AI
import Gomoku.InputParser

import Data.Time.Clock
import Data.Char

import Gomoku.BitBoard.Helpers

main :: IO ()
main = humanMove newGame Black

humanMove gameState player = do
    let GameState board _ _ = gameState
    putStrLn $ show board
    putStrLn ("Player " ++ show player)
    line <- getLine
    let playerMove = parseMove player line
    putStrLn $ show playerMove
    let newGameState = updateGameState gameState playerMove
    continueGame newGameState player aiMove

aiMove gameState player = do
    let GameState board _ _ = gameState
    putStrLn $ show board
    putStrLn ("Player " ++ show player)
    t1 <- getCurrentTime
    let aiMove = minimax (movesTreeOnlyBest 9) 4 gameState
    putStrLn $ "Best AI move: " ++ show aiMove
    t2 <- getCurrentTime
    let elapsed = diffUTCTime t2 t1
    putStrLn $ show elapsed
    let newGameState = updateGameState gameState aiMove
    return ()
    continueGame newGameState player humanMove

continueGame gameState player nextMoveFun = do
    if gameIsOver $ evaluation gameState
    then do
        putStrLn "Game Over"
        return ()
    else do
        putStrLn $ show $ evaluation gameState
        let blackScore = score $ black $ evaluation gameState
        let whiteScore = score $ white $ evaluation gameState
        putStrLn $ "Black score: " ++ (show blackScore)
        putStrLn $ "White score: " ++ (show whiteScore)
        nextMoveFun gameState $ otherPlayer player
