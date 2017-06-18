module Gomoku.Threats where

import Gomoku.Abstractions
import Gomoku.Patterns

import qualified Data.List as L

printPatternsCount =
    L.concat $
    L.intersperse "\n" $
    [ print pattern patternType|
        patternType <- [(minBound :: PatternType)..(maxBound :: PatternType)],
        pattern <- [Five, StraightFour, Four, Three, Double] ]
    where
        print pattern patternType = (show patternType) ++ "\t" ++ (show pattern) ++ "\t\t" ++ (show $ length $ genPatterns Black patternType pattern)

printPatternsPos =
    L.concat $
    L.intersperse "\n" $
    zipWith (\a b -> (show $ fst a) ++ " " ++ (show $ snd a) ++ " = " ++ (show b)) plist ilist
    where
        llist = L.map (\(patternType, pattern) -> L.length $ genPatterns Black patternType pattern) plist
        plist = [ (patternType, pattern) |
            patternType <- [(minBound :: PatternType)..(maxBound :: PatternType)],
            pattern <- [Five, StraightFour, Four, Three, Double] ]
        slist = scanl (\start len -> start + len) 0 llist
        ilist = zipWith (\s p -> (s,p)) slist llist

