{-# OPTIONS_GHC -fplugin=LiquidHaskell #-}

module Demo where

import Data.Set

{-@ type Pos = {v:Int | 0 < v} @-}

{-@ incr :: Pos -> Pos @-}
incr :: Int -> Int
incr x = x + 1


{-@ measure elts @-}
elts        :: (Ord a) => [a] -> Set a
elts []     = empty
elts (x:xs) = singleton x `union` elts xs

{-@ rev :: xs:_ -> {v:_ | elts v == elts xs} @-}
rev :: [a] -> [a]
rev = go []
  where
    {-@ go :: acc:_ -> xs:_ -> {v:_ | elts v == union (elts acc) (elts xs)} @-}
    go acc []     = acc
    go acc (x:xs) = go (x:acc) xs


-- demo :: Int
-- demo = incr (-1)
--
-- LZ77.hs:29:13: error:
--     Liquid Type Mismatch
--     .
--     The inferred type
--       VV : {v : GHC.Types.Int | v == (-(1 : int))}
--     .
--     is not a subtype of the required type
--       VV : {VV##754 : GHC.Types.Int | 0 < VV##754}
--     .
--     Constraint id 1
--    |
-- 29 | demo = incr (-1)
