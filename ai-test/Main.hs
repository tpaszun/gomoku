module Main where

import Gomoku.Abstractions
import Gomoku.AI
import Gomoku.GameState

import Data.Tree
import Data.Tree.Pretty

main :: IO ()
main = do
    -- let gameState = createGameState 19 exampleGame
    let gameState = createGameState 19 [Move 9 9 Black]
    -- let gameState = createGameState 19 threatBoard2
    simulation gameState

simulation :: GameState -> IO ()
simulation gameState = do
    putStrLn $ show $ board gameState
    putStrLn $ show $ evaluation gameState
    putStrLn ("Total score: " ++ (show $ totalScore gameState))
    let (Move _ _ lastPlayer) = head $ moves gameState
    let depth = case lastPlayer of
                  Black -> 2
                  White -> 4
    let move = minimax (movesTreeOnlyBest 10) depth gameState
    putStrLn $ show move
    let updatedGameState = updateGameState gameState move
    if gameIsOver $ evaluation updatedGameState
        then do
            putStrLn $ show $ board updatedGameState
            putStrLn "Game Over"
            let (Move _ _ player) = move
            putStrLn ("Winner: " ++ (show player))
            return ()
        else simulation updatedGameState

treeToLevel :: Int -> Tree a -> Tree a
treeToLevel 0 tree =
    Node {
        rootLabel = rootLabel tree,
        subForest = []
    }
treeToLevel level tree =
    Node {
        rootLabel = rootLabel tree,
        subForest = fmap (treeToLevel (level - 1)) $ subForest tree
    }

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
