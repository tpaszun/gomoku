module Main (
    main
) where

import Gomoku.Abstractions
import Gomoku.AI
import Gomoku.InputParser

import Data.Time.Clock


main :: IO ()
main = humanMove newGame Black

humanMove :: GameState -> Player -> IO ()
humanMove gameState player = do
    putStrLn $ show $ board gameState
    putStrLn ("Player " ++ show player)
    line <- getLine
    let playerMove = parseMove player line
    putStrLn $ show playerMove
    let newGameState = updateGameState gameState playerMove
    continueGame newGameState player aiMove

aiMove :: GameState -> Player -> IO ()
aiMove gameState player = do
    putStrLn $ show $ board gameState
    putStrLn ("Player " ++ show player)
    t1 <- getCurrentTime
    let move = minimax (movesTreeOnlyBest 9) 4 gameState
    putStrLn $ "Best AI move: " ++ show move
    t2 <- getCurrentTime
    let elapsed = diffUTCTime t2 t1
    putStrLn $ show elapsed
    let newGameState = updateGameState gameState move
    return ()
    continueGame newGameState player humanMove

continueGame :: GameState -> Player -> (GameState -> Player -> IO ()) -> IO ()
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
