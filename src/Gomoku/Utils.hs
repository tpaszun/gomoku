module Gomoku.Utils where

import qualified Data.Set as S

-- remove duplicates from list
rmdups :: Ord a => [a] -> [a]
rmdups = S.toList . S.fromList