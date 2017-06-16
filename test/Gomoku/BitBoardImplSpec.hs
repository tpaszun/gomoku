module Gomoku.BitBoardImplSpec where

import Test.Hspec

import Gomoku.Abstractions
import Gomoku.BitBoard
import Gomoku.BitBoardImpl
import Gomoku.BitBoard.Helpers

import ExampleBoards

spec :: Spec
spec = do
  updateBoardSpec
  -- intersectionSpec
  -- evaluateIntersectionSpec

updateBoardSpec :: Spec
updateBoardSpec =
  describe "updateBoard" $ do
    let b = blankBoard 4::BitBoard

    updateBoardTest b (Move 0 0 Black) (Expected {
      horiz = (0,0),
      vert  = (0,0),
      diagL = (0,0),
      diagR = (0,3)
    })

    updateBoardTest b (Move 0 1 Black) (Expected {
      horiz = (0,1),
      vert  = (1,0),
      diagL = (1,1),
      diagR = (0,4)
    })

    updateBoardTest b (Move 0 3 Black) (Expected {
      horiz = (0,3),
      vert  = (3,0),
      diagL = (3,3),
      diagR = (0,6)
    })

    updateBoardTest b (Move 1 0 Black) (Expected {
      horiz = (1,0),
      vert  = (0,1),
      diagL = (0,1),
      diagR = (0,2)
    })

    updateBoardTest b (Move 1 3 Black) (Expected {
      horiz = (1,3),
      vert  = (3,1),
      diagL = (2,4),
      diagR = (1,5)
    })

    updateBoardTest b (Move 2 3 Black) (Expected {
      horiz = (2,3),
      vert  = (3,2),
      diagL = (1,5),
      diagR = (2,4)
    })

    updateBoardTest b (Move 3 0 Black) (Expected {
      horiz = (3,0),
      vert  = (0,3),
      diagL = (0,3),
      diagR = (0,0)
    })

    updateBoardTest b (Move 3 2 Black) (Expected {
      horiz = (3,2),
      vert  = (2,3),
      diagL = (0,5),
      diagR = (2,2)
    })

    updateBoardTest b (Move 3 3 Black) (Expected {
      horiz = (3,3),
      vert  = (3,3),
      diagL = (0,6),
      diagR = (3,3)
    })

data Expected = Expected {
  horiz :: (Int,Int),
  vert :: (Int, Int),
  diagL :: (Int,Int),
  diagR :: (Int,Int)
}

updateBoardTest :: BitBoard -> Move -> Expected -> Spec
updateBoardTest board move expected =
  describe ("with " ++ show move) $ do
      let b = updateBoard board move
      it ("should update horizontal field " ++ (show $ horiz expected)) $ do
        getFieldHorizontal b (horiz expected) `shouldBe` (Player Black)
      it ("should update vertical field " ++ (show $ vert expected)) $ do
        getFieldVertical b (vert expected) `shouldBe` (Player Black)
      it ("should update left diagonals field " ++ (show $ diagL expected)) $ do
        getFieldDiagonalL b (diagL expected) `shouldBe` (Player Black)
      it ("should update right diagonals field " ++ (show $ diagR expected)) $ do
        getFieldDiagonalR b (diagR expected) `shouldBe` (Player Black)

intersectionSpec :: Spec
intersectionSpec =
  describe "intersection" $ do
    let b = exampleBoardFull
    it "should create intersection" $ do
      let inters = intersection b (9,10)
      putStrLn $ showBoardAsBinary  b
      putStrLn $ showIntersection inters

evaluateIntersectionSpec :: Spec
evaluateIntersectionSpec =
  describe "evaluateIntersection" $ do
    let b = exampleBoardFull
    it "should evaluate intersection" $ do
      putStrLn $ show $ evaluateIntersection b (9,11)
    it "should calculate dif" $ do
      let firstEval = evaluateIntersection b (10,11)
      putStrLn $ show firstEval
      let b' = updateBoard b (Move 10 11 Black)
      let secondEval = evaluateIntersection b' (10,11)
      putStrLn $ show secondEval
      putStrLn $ show $ dif secondEval firstEval
    it "should calculate dif" $ do
      let firstEval = evaluateIntersection b (10,11)
      putStrLn $ show firstEval
      let b' = updateBoard b (Move 10 11 White)
      let secondEval = evaluateIntersection b' (10,11)
      putStrLn $ show secondEval
      putStrLn $ show $ dif secondEval firstEval
