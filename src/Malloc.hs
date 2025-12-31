{-# LANGUAGE QualifiedDo, BlockArguments, RebindableSyntax, TypeApplications, UnliftedNewtypes, RankNTypes, PolyKinds, DataKinds, TypeFamilies, TypeOperators, UndecidableInstances, MagicHash #-}
module Malloc where
import GHC.Types
import GHC.TypeLits (TypeError, ErrorMessage(..))
import Prelude (fromInteger, (.))
import Prelude qualified
import Foreign.Ptr qualified as Foreign
import Foreign.Marshal.Alloc qualified as Foreign
import Foreign.Storable qualified as Foreign
import Foreign.Storable (Storable)

type family Has (x :: k) (xs :: [k]) :: Constraint where
  Has x '[] = TypeError ('Text "Missing type: " ':<>: 'ShowType x)
  Has x (x ': xs) = ()
  Has x (y ': xs) = Has x xs

type family Delete (x :: k) (xs :: [k]) :: [k] where
  Delete x '[] = '[]
  Delete x (x ': xs) = Delete x xs
  Delete x (y ': xs) = y ': Delete x xs

--------------------------------------------------------------------------------

data Pointer

newtype M (ps :: [Pointer]) (ps' :: [Pointer]) (a :: Type) = M { runM :: IO a }

infixl 1 >>
(>>) :: M p1 p2 a -> M p2 p3 b -> M p1 p3 b
M m >> M m' = M (m Prelude.>> m')

infixl 1 >>=
(>>=) :: M p1 p2 a -> (a -> M p2 p3 b) -> M p1 p3 b
M m >>= m' = M (m Prelude.>>= runM . m')

pure :: a -> M ps ps' a
pure = M . Prelude.pure

newtype Ptr (p :: Pointer) (a :: Type) = Ptr { unPtr :: Foreign.Ptr a }

withMalloc :: Int -> (forall p. Has p (p:ps) => Ptr p a -> M (p : ps) (Delete p ps) b) -> M ps ps b
withMalloc n f = M (Prelude.do
  ptr <- Foreign.mallocBytes n
  v <- runM (f (Ptr ptr))
  Foreign.free ptr
  Prelude.pure v)

free :: Has p ps => Ptr p a -> M ps (Delete p ps) ()
free ptr = M (Foreign.free (unPtr ptr))

poke :: (Storable a,Has p ps) => a -> Ptr p a -> M ps ps ()
poke a ptr = M (Foreign.poke (unPtr ptr) a)

peek :: (Storable a, Has p ps) => Ptr p a -> M ps ps a
peek ptr = M (Foreign.peek (unPtr ptr))
