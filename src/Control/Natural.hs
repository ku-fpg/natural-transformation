{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleInstances, FunctionalDependencies,
             GADTs, MultiParamTypeClasses, RankNTypes, TypeOperators #-}

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Safe #-}
#endif

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 706 && MIN_VERSION_base(4,7,0)
# define LANGUAGE_PolyKinds
{-# LANGUAGE PolyKinds #-}
#endif

{-|
Module:      Control.Natural
Copyright:   (C) 2015 The University of Kansas
License:     BSD-style (see the file LICENSE)
Maintainer:  Andy Gill
Stability:   Experimental

A data type and type class for natural transformations.
-}
module Control.Natural
  ( (~>)()
  , (:~>)(..)
  , Transformation(..)
  ) where

#if defined(LANGUAGE_PolyKinds)
import qualified Control.Category as C (Category(..))
#endif

#if !(MIN_VERSION_base(4,8,0))
import           Data.Monoid (Monoid(..))
#endif
import           Data.Typeable (Typeable)

infixr 0 #
-- | A (natural) transformation is inside @t@, and contains @f@ and @g@
-- (typically 'Functor's).
-- 
-- The order of arguments allows the use of @GeneralizedNewtypeDeriving@ to wrap
-- a 'Natural', but maintain the 'Transformation' constraint. Thus, '#' can be used
-- on abstract data types.
class Transformation f g t | t -> f g where
    -- | The invocation method for a natural transformation.
    (#) :: t -> f a -> g a

instance Transformation f g (f :~> g) where
    Nat f # g = f g

-- Code adapted from Edward Kmett's @indexed@ package

infixr 0 ~>
-- | A natural transformation from @f@ to @g@.
type f ~> g = forall x. f x -> g x

infixr 0 :~>, $$
-- | A data type representing a natural transformation from @f@ to @g@.
newtype f :~> g = Nat { ($$) :: f ~> g } deriving Typeable

#if defined(LANGUAGE_PolyKinds)
instance C.Category (:~>) where
    id = Nat id
    Nat f . Nat g = Nat (f . g)
#endif

instance f ~ g => Monoid (f :~> g) where
    mempty = Nat id
    mappend (Nat f) (Nat g) = Nat (f . g)
