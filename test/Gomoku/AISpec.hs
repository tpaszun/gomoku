module Gomoku.AISpec where

import Test.Hspec

import Gomoku.Abstractions
import Gomoku.BitBoard
import Gomoku.AI
import Gomoku.GameState

import ExampleBoards

spec :: Spec
spec =
  describe "threat board 2" $ do
    let game = createGameState 15 threatBoard2
    minimaxTest game 2
    -- minimaxTest game 3
    -- minimaxTest game 4
    -- minimaxTest game 5

minimaxTest :: GameState -> Int -> Spec
minimaxTest game depth =
  it ("minimax with depth " ++ show depth) $ do
    let m = minimax (movesTreeOnlyBest 5) depth game
    putStrLn $ show m