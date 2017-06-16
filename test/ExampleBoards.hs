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

exampleBoardFull :: BitBoard
exampleBoardFull = foldl (updateBoard) (blankBoard 19) moves
    where
        moves = [
                    (Move 9 9 Black),
                    (Move 10 10 White),
                    (Move 10 9 Black),
                    (Move 11 9 White),
                    (Move 9 10 Black),
                    (Move 9 11 White),
                    (Move 8 12 Black),
                    (Move 8 11 White)
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
