{-# OPTIONS_GHC -fplugin=LiquidHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
-- |

module DivByZero where

{-@ divide :: Int -> { d : Int | d /= 0 } -> Int @-}
divide :: Int -> Int -> Int
divide x n = x `div` n

bad :: IO ()
bad = do
  d <- readLn
  if d /= 0
     then print (divide 10 d)
     else putStrLn "Please try again."

{-@ impossible :: {v:_ | false} -> a @-}
impossible :: String -> a
impossible msg = error msg
