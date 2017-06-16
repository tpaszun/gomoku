module Main where

import Gomoku.Abstractions
import Gomoku.AI

import Criterion.Main

main :: IO ()
main = defaultMain [
    benchmarkThreatBoard "threat board 2 (best 3 moves evaluation)" 5 (movesTreeOnlyBest 3) threatBoard2,
    benchmarkThreatBoard "threat board 1 (best 3 moves evaluation)" 5 (movesTreeOnlyBest 3) threatBoard1,
    benchmarkThreatBoard "threat board 2 (best 5 moves evaluation)" 5 (movesTreeOnlyBest 5) threatBoard2,
    benchmarkThreatBoard "threat board 1 (best 5 moves evaluation)" 5 (movesTreeOnlyBest 5) threatBoard1,
    benchmarkThreatBoard "threat board 2 (best 10 moves evaluation)" 5 (movesTreeOnlyBest 10) threatBoard2,
    benchmarkThreatBoard "threat board 1 (best 10 moves evaluation)" 5 (movesTreeOnlyBest 10) threatBoard1,
    benchmarkThreatBoard "threat board 2 (intersection evaluation)" 2 movesTreeInters threatBoard2,
    benchmarkThreatBoard "threat board 1 (intersection evaluation)" 2 movesTreeInters threatBoard1,
    benchmarkThreatBoard "threat board 2" 2 movesTree threatBoard2,
    benchmarkThreatBoard "threat board 1" 1 movesTree threatBoard1
  ]

benchmarkThreatBoard :: String -> Int -> GameTreeGenerator -> [Move] -> Benchmark
benchmarkThreatBoard name maxDepth treeGen ms =
  bgroup ("minimax - " ++ name)
    [
      bench ("depth " ++ show depth)  $ whnf (\depth -> minimax (treeGen) depth game) 1 | depth <- [1..maxDepth]
    ]
  where
    game = createGameState 15 ms

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
