module Main where

import Gomoku.Abstractions
import Gomoku.AI
import Gomoku.GameState
import Gomoku.ThreatSearch

import Data.Maybe

import Data.Time.Clock

main :: IO ()
main = do
    -- let gameState = createGameState 19 exampleGame
    let gameState = createGameState 19 [Move 9 9 Black]
    -- let gameState = createGameState 15 threatBoard2
    -- let gameState = createGameState 19 blackSureWin
    simulation gameState

simulation :: GameState -> IO ()
simulation gameState = do
    putStrLn $ show $ board gameState
    putStrLn $ show $ evaluation gameState
    putStrLn ("Total score: " ++ (show $ totalScore gameState))
    let (Move _ _ lastPlayer) = head $ moves gameState
    let currentPlayer = otherPlayer lastPlayer
    let currentAgent = case currentPlayer of
                        Black -> blackAgent
                        White -> whiteAgent
    t1 <- getCurrentTime
    move <- currentAgent gameState
    putStrLn $ show move
    t2 <- getCurrentTime
    putStrLn $ "Total move search elapsed time: " ++ (show $ diffUTCTime t2  t1)
    let updatedGameState = updateGameState gameState move
    if gameIsOver $ evaluation updatedGameState
        then do
            putStrLn $ show $ board updatedGameState
            putStrLn "Game Over"
            let (Move _ _ player) = move
            putStrLn ("Winner: " ++ (show player))
            return ()
        else simulation updatedGameState


blackAgent :: GameState -> IO Move
blackAgent game = do
    return $ minimax (movesTreeOnlyBest 6) 5 game

whiteAgent :: GameState -> IO Move
whiteAgent game = do
    t1 <- getCurrentTime
    let sureWin = listToMaybe $ winningThreatSequence game 8
    putStrLn $ case sureWin of
            Nothing -> "sure win not found"
            Just move -> "sure win found: " ++ (show move)
    t2 <- getCurrentTime
    putStrLn $ ("sure win search time: " ++ (show $ diffUTCTime t2 t1))
    case sureWin of
        Just move -> return $ move
        Nothing ->
            return $ minimax (movesTreeOnlyBest 6) 5 game




threatBoard1 :: [Move]
threatBoard1 = [
    (Move 0 0 Black),
    (Move 0 1 Black),
    (Move 0 2 Black),
    (Move 1 0 Black),
    (Move 2 0 Black),

    (Move 0 12 Black),
    (Move 0 13 Black),
    (Move 0 14 Black),
    (Move 1 14 Black),
    (Move 2 14 Black),

    (Move 12 0 Black),
    (Move 13 0 Black),
    (Move 14 0 Black),
    (Move 14 1 Black),
    (Move 14 2 Black),

    (Move 12 14 Black),
    (Move 13 14 Black),
    (Move 14 14 Black),
    (Move 14 13 Black),
    (Move 14 12 Black),

    (Move 5 1 Black),
    (Move 6 2 Black),

    (Move 8 5 Black),
    (Move 8 6 Black),

    (Move 8 2 White),
    (Move 9 5 White),
    (Move 8 9 White)]

threatBoard2 :: [Move]
threatBoard2 = [
    (Move 6 4 Black),

    (Move 6 7 White),
    (Move 7 7 Black),

    (Move 5 8 White),
    (Move 6 8 Black),
    (Move 7 8 Black),

    (Move 5 9 Black),
    (Move 6 9 White),
    (Move 7 9 Black),
    (Move 11 9 White),

    (Move 4 10 White),
    (Move 7 10 White)]

threatBoard3 :: [Move]
threatBoard3 = [
    (Move 6 4 Black),

    (Move 6 7 White),
    (Move 7 7 Black),

    (Move 5 8 White),
    (Move 6 8 Black),
    (Move 7 8 Black),

    (Move 5 9 Black),
    (Move 6 9 White),
    (Move 7 9 Black),
    (Move 11 9 White),

    (Move 4 10 White),
    (Move 7 10 White),
    (Move 7 5 Black)]

exampleGame :: [Move]
exampleGame = [
    Move 9 9 Black,
    Move 10 10 White,
    Move 11 10 Black,
    Move 9 8 White,
    Move 12 9 Black,
    Move 13 8 White,
    Move 11 9 Black,
    Move 10 9 White,
    Move 10 8 Black,
    Move 11 7 White,
    Move 12 10 Black,
    Move 13 11 White,
    Move 9 7 Black]

blackSureWin :: [Move]
blackSureWin = [
    (Move 9 9 Black),
    (Move 8 8 White),
    (Move 8 9 Black),
    (Move 7 9 White),
    (Move 9 10 Black),
    (Move 9 7 White),
    (Move 6 10 Black),
    (Move 7 8 White),
    (Move 9 11 Black),
    (Move 9 8 White),
    (Move 10 8 Black),
    (Move 7 10 White)]