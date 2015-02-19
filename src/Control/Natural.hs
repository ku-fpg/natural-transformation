{-# LANGUAGE CPP, FlexibleInstances, GADTs,
             MultiParamTypeClasses, RankNTypes, TypeOperators #-}

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 706 && MIN_VERSION_base(4,7,0)
# define LANGUAGE_PolyKinds
{-# LANGUAGE PolyKinds #-}
#endif

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 708
# define LANGUAGE_DeriveDataTypeable
{-# LANGUAGE DeriveDataTypeable #-}
#endif

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
# if defined(LANGUAGE_DeriveDataTypeable)
{-# LANGUAGE Safe #-}
# else
{-# LANGUAGE Trustworthy #-}
# endif
#endif

{-|
Module:      Control.Natural
Copyright:   (C) 2015 The University of Kansas
License:     BSD-style (see the file LICENSE)
Maintainer:  Andy Gill
Stability:   Experimental

A data type for natural transformations.
-}
module Control.Natural ((:~>)(..)) where

#if defined(LANGUAGE_PolyKinds)
import qualified Control.Category as C (Category(..))
#endif
import           Control.Transformation (Transformation(..))

#if !(MIN_VERSION_base(4,8,0))
import           Data.Monoid (Monoid(..))
#endif
import           Data.Typeable (Typeable)
#if !defined(LANGUAGE_DeriveDataTypeable)
import           Data.Typeable (TyCon, Typeable1(..), mkTyCon3, mkTyConApp, typeOf)
#endif

-------------------------------------------------------------------------------
-- Code adapted from Edward Kmett's @indexed@ package
-------------------------------------------------------------------------------

infixr 0 :~>, $$
-- | A natural transformation from @f@ to @g@.
newtype f :~> g = Nat { ($$) :: forall x. f x -> g x }
#if defined(LANGUAGE_DeriveDataTypeable)
  deriving Typeable
#else
instance (Typeable1 f, Typeable1 g) => Typeable (f :~> g) where
    typeOf _ = mkTyConApp natTyCon [typeOf1 (undefined :: f a), typeOf1 (undefined :: g a)]

natTyCon :: TyCon
natTyCon = mkTyCon3 "natural-transformation" "Control.Natural" ":~>"
{-# NOINLINE natTyCon #-}
#endif

#if defined(LANGUAGE_PolyKinds)
instance C.Category (:~>) where
    id = Nat id
    Nat f . Nat g = Nat (f . g)
#endif

instance f ~ g => Monoid (f :~> g) where
    mempty = Nat id
    mappend (Nat f) (Nat g) = Nat (f . g)

instance Transformation f g (f :~> g) where
    Nat f # g = f g