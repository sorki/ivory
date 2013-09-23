{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Ivory.Language.DynArray where

import GHC.TypeLits

import Ivory.Language.Area
import Ivory.Language.Array
import Ivory.Language.Effects
import Ivory.Language.IBool
import Ivory.Language.Init
import Ivory.Language.Monad
import Ivory.Language.Proxy
import Ivory.Language.Scope
import Ivory.Language.Ref
import Ivory.Language.Type

import qualified Ivory.Language.Syntax as I

-- Dynamic Array Primitives

-- | Allocate a dynamic array wrapping an existing array.
toDynArray :: forall a s1 s2 eff len ref.
              ( SingI len, IvoryArea a, IvoryRef ref
              , GetAlloc eff ~ Scope s2
              , IvoryExpr (ref s1 (Array len a))
              , IvoryExpr (ref (Stack s2) (DynArray a)))
           => ref s1 (Array len a)
           -> Ivory eff (ref (Stack s2) (DynArray a))
toDynArray ref = do
  let ty = ivoryType (Proxy :: Proxy (ref (Stack s2) (DynArray a)))
  ref' <- local (idynarray ref)
  return (wrapExpr (I.ExpSafeCast ty (unwrapExpr ref')))

-- | Use a dynamic array as a sized array, calling an error
-- handler function if the array is too small.
withDynArray :: forall a b c s eff len ref.
                ( SingI len, IvoryArea a, IvoryRef ref
                , IvoryExpr (ref s (DynArray a))
                , IvoryExpr (ref s (Array len a)))
             => ref s (DynArray a)
             -> (ref s (Array len a) -> IxRep -> Ivory eff b)
             -> (IxRep -> Ivory eff c)
             -> Ivory eff ()
withDynArray arr ok_f fail_f = do
  let req_len = fromInteger (fromTypeNat (sing :: Sing len))
  let ty = ivoryType (Proxy :: Proxy (ref s (Array len a)))
  len <- deref (dynArrayLength arr)
  ifte_ (req_len <=? len)
    (do a <- assign (wrapExpr (I.ExpDynArrayData ty (unwrapExpr arr)))
        ok_f a len)
    (fail_f len)

-- | Retrieve a reference to a dynamic array's length.
dynArrayLength :: ( IvoryArea a, IvoryRef ref
                  , IvoryExpr (ref s (DynArray a)))
               => ref s (DynArray a)
               -> ConstRef s (Stored IxRep)
dynArrayLength d = wrapExpr (I.ExpDynArrayLength (unwrapExpr d))

-- | Map a function over each element of a dynamic array.
dynArrayMap :: forall a b s eff ref.
               ( IvoryArea a, IvoryRef ref
               , IvoryExpr (ref s (DynArray a))
               , IvoryExpr (ref s a))
            => ref s (DynArray a)
            -> (ref s a -> IxRep -> Ivory eff b)
            -> Ivory eff ()
dynArrayMap arr body = do
  elt        <- freshVar "elt"
  ix         <- freshVar "ix"
  let eltVar  = wrapExpr (I.ExpVar elt)
  let ixVar   = wrapExpr (I.ExpVar ix)
  (_, block) <- collect (body eltVar ixVar)
  let ty      = ivoryType (Proxy :: Proxy (ref s (DynArray a)))
  emit (I.DynArrayMap elt ix ty (unwrapExpr arr) (blockStmts block))


-- TODO: Define a version of "withDynArray" that is polymorphic in the
-- array length?

