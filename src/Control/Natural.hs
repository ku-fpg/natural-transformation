{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeOperators #-}

{-|
Module:      Control.Natural
Copyright:   (C) 2015 The University of Kansas
License:     BSD-style (see the file LICENSE)
Maintainer:  Andy Gill
Stability:   Experimental

A data type and class for natural transformations.
-}
module Control.Natural
  ( -- * Newtype for a Natural Transformation
    (:~>)(..)
    -- * Type Synonym for a Natural Transformation
  , type (~>)
    -- * Conversion functions between the newtype and the synonym
  , run
  , nat
    -- * Class for Natural Transformations
  , Transformation(..)
  ) where

import qualified Control.Category as C (Category(..))

#if !(MIN_VERSION_base(4,8,0))
import           Data.Monoid (Monoid(..))
#endif
#if MIN_VERSION_base(4,9,0)
import           Data.Semigroup (Semigroup(..))
#endif
import           Data.Typeable

---------------------------------------------------------------------------
-- Naming of ~>, :~> and $$ are taken (with permission) from Edward Kmett's @indexed@ package.
---------------------------------------------------------------------------

infixr 0 ~>
-- | A natural transformation from @f@ to @g@.
type f ~> g = forall x. f x -> g x

infixr 0 :~>, $$
-- | A natural transformation suitable for storing in a container.
newtype f :~> g = Nat { ($$) :: f ~> g }
  deriving Typeable

instance C.Category (:~>) where
    id = Nat id
    Nat f . Nat g = Nat (f . g)

#if MIN_VERSION_base(4,9,0)
instance f ~ g => Semigroup (f :~> g) where
    Nat f <> Nat g = Nat (f . g)
#endif

instance f ~ g => Monoid (f :~> g) where
    mempty = Nat id
    mappend (Nat f) (Nat g) = Nat (f . g)

infix 0 #
-- | A (natural) transformation is inside @t@, and contains @f@ and @g@
-- (typically 'Functor's).
--
-- The order of arguments allows the use of @GeneralizedNewtypeDeriving@ to wrap
-- a ':~>', but maintain the 'Transformation' constraint. Thus, @#@ can be used
-- on abstract data types.
class Transformation f g t | t -> f g where
    -- | The invocation method for a natural transformation.
    (#) :: t -> forall a . f a -> g a

instance Transformation f g (f :~> g) where
    Nat f # g = f g

-- | 'run' is the nonfix version of @#@. It is used to break natural
--   transformation wrappers, including ':~>'.
run :: Transformation f g t => t -> (forall a . f a -> g a)
run = (#)

-- | 'nat' builds our natural transformation abstraction out of
--    a natural transformation function.
nat :: (forall a . f a -> g a) -> f :~> g
nat = Nat
