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
  , wrapNT
  , unwrapNT
    -- * Type inference helper
  , applyNT
    -- * Class for Natural Transformations
  , Transformation(..)
  ) where

import qualified Control.Category as C (Category(..))

#if !(MIN_VERSION_base(4,8,0))
import           Data.Monoid (Monoid(..))
#endif
import           Data.Semigroup (Semigroup(..))
import           Data.Typeable

---------------------------------------------------------------------------
-- Naming of ~>, :~> and $$ are taken (with permission) from Edward Kmett's @indexed@ package.
---------------------------------------------------------------------------

infixr 0 ~>
-- | A natural transformation from @f@ to @g@.
type f ~> g = forall x. f x -> g x

infixr 0 :~>, $$
-- | A natural transformation suitable for storing in a container.
newtype f :~> g = NT { ($$) :: f ~> g }
  deriving Typeable

instance C.Category (:~>) where
    id = NT id
    NT f . NT g = NT (f . g)

instance f ~ g => Semigroup (f :~> g) where
    NT f <> NT g = NT (f . g)

instance f ~ g => Monoid (f :~> g) where
    mempty = NT id
    mappend = (<>)

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
    NT f # g = f g

-- | 'wrapNT' builds our natural transformation abstraction out of
-- a natural transformation function.
--
-- An alias to 'NT' provided for symmetry with 'unwrapNT'.
--
wrapNT :: (forall a . f a -> g a) -> f :~> g
wrapNT = NT

-- | 'applyNT' is the nonfix version of @#@. It is used to break natural
--   transformation wrappers, including ':~>'.
unwrapNT :: Transformation f g t => t -> (forall a . f a -> g a)
unwrapNT = (#)
--
-- See <https://github.com/ku-fpg/natural-transformation/issues/9#issuecomment-172121060 comment on GitHub>
applyNT :: (f ~> g) -> (f ~> g)
applyNT nt = nt
