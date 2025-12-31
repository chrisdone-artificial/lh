{-# LANGUAGE QualifiedDo, BlockArguments, TypeApplications, UnliftedNewtypes, RankNTypes, PolyKinds, DataKinds, TypeFamilies, TypeOperators, UndecidableInstances, MagicHash #-}

import Malloc

demo :: M '[] '[] ()
demo = withMalloc 5 \ptr -> Malloc.do
  poke (1::Int) ptr
  thing <- peek ptr
  -- free ptr -- Causes a type error of "Missing type: p"
  peek ptr
  Malloc.pure ()
