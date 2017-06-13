module ExampleBoards where

import Gomoku.Abstractions
import Gomoku.BitBoard
import Gomoku.BitBoardImpl()


exampleBoard :: BitBoard
exampleBoard = foldl (updateBoard) (blankBoard 19) moves
    where
        moves = [
                    (Move 3 3 Black),
                    (Move 4 4 White),
                    (Move 2 4 Black),
                    (Move 3 4 White),
                    (Move 4 2 Black),
                    (Move 3 5 White),
                    (Move 5 1 Black),
                    (Move 2 3 White)
                ]

exampleBoardPre :: BitBoard
exampleBoardPre = foldl (updateBoard) (blankBoard 8) moves
    where
        moves = [
                    (Move 0 0 Black),
                    (Move 0 1 White),
                    (Move 1 0 Black),
                    (Move 1 1 White),
                    (Move 2 0 Black),
                    (Move 2 1 White),
                    (Move 3 0 Black),
                    (Move 3 1 White),

                    (Move 0 2 Black),
                    (Move 0 3 White),
                    (Move 1 2 Black),
                    (Move 1 3 White),
                    (Move 2 2 Black),
                    (Move 2 3 White)
                ]

exampleBoardSuf :: BitBoard
exampleBoardSuf = foldl (updateBoard) (blankBoard 8) moves
    where
        moves = [
                    (Move 7 0 Black),
                    (Move 7 1 White),
                    (Move 6 0 Black),
                    (Move 6 1 White),
                    (Move 5 0 Black),
                    (Move 5 1 White),
                    (Move 4 0 Black),
                    (Move 4 1 White),

                    (Move 7 2 Black),
                    (Move 7 3 White),
                    (Move 6 2 Black),
                    (Move 6 3 White),
                    (Move 5 2 Black),
                    (Move 5 3 White)
                ]

exampleBoardEx :: BitBoard
exampleBoardEx = foldl (updateBoard) (blankBoard 8) moves
    where
        moves = [
                    (Move 4 0 Black),
                    (Move 3 1 Black),
                    (Move 2 2 Black),
                    (Move 1 3 Black)
                ]

exampleBoard5 :: BitBoard
exampleBoard5 = foldl (updateBoard) (blankBoard 5) moves
    where
        moves = [
                    (Move 0 0 Black),
                    (Move 0 1 White),
                    (Move 0 3 Black),
                    (Move 0 2 White),

                    (Move 1 0 Black),
                    (Move 1 2 White),
                    (Move 1 1 Black),

                    (Move 2 1 Black),
                    (Move 4 0 White),
                    (Move 2 2 Black),


                    (Move 3 0 White)
                ]