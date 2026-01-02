{-# OPTIONS_GHC -fplugin=LiquidHaskell #-}
{-# language OverloadedStrings, BangPatterns, MagicHash, BlockArguments, UnliftedFFITypes, UnboxedTuples #-}
import Data.Word ()
import Control.Monad.Primitive
import Data.Primitive.ByteArray
import GHC.Prim
import Foreign.C.Types
import System.Posix.IO.ByteString
import System.Posix.Types

main :: IO ()
main = do
  fd@(Fd fd') <- openFd "/home/chris/Work/chrisdone/lh/.stack-work/stack.sqlite3" ReadOnly defaultFileFlags
  let bufsize :: CSize
      bufsize = (128*1024)
  buffer <- newPinnedByteArray' (fromIntegral bufsize)
  keepAlive buffer do
    let loop :: Int -> IO ()
        loop 0 = pure ()
        loop (!i) = do
          count <- c_safe_read fd' (mutableByteArrayContents# (unMutableByteArray# buffer)) bufsize
          if count == 0 || count == (-1)
             then pure ()
             else do
               loop (i-1)
    loop 100000
    closeFd fd

--------------------------------------------------------------------------------
-- Assumptions

{-@ assume newPinnedByteArray' :: l:Int -> IO ({a:MutableByteArray RealWorld|mlen a==l}) @-}
newPinnedByteArray' :: Int -> IO (MutableByteArray RealWorld)
newPinnedByteArray' = newPinnedByteArray

{-@ assume unMutableByteArray# :: a:(MutableByteArray s) -> {b:MutableByteArray# s| mlen a==mlen# b} @-}
unMutableByteArray# :: MutableByteArray s -> MutableByteArray# s
unMutableByteArray# (MutableByteArray a) = a

{-@ assume mutableByteArrayContents# :: m:(MutableByteArray# s) -> {a : Addr# | alen# a == mlen# m } @-}

{-@
  assume c_safe_read ::
     CInt ->
     p:Addr# ->
     { s : CSize | s > 0 && s <= alen# p } ->
     IO { c : CSize | c >= -1 && c <= s }
  @-}
foreign import ccall safe "read"
   c_safe_read :: CInt -> Addr# -> CSize -> IO CSize

--------------------------------------------------------------------------------
-- Measures

{-@ measure alen# :: Addr# -> Int @-}
{-@ measure mlen :: MutableByteArray s -> Int @-}
{-@ measure mlen# :: MutableByteArray# s -> Int @-}
