{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeOperators #-}

#if MIN_VERSION_base(4,7,0)
{-# LANGUAGE PolyKinds #-}
#endif

{-|
Module:      Control.Transformation
Copyright:   (C) 2015 The University of Kansas
License:     BSD-style (see the file LICENSE)
Maintainer:  Andy Gill
Stability:   Experimental

A type class for transformations.
-}
module Control.Transformation (Transformation(..)) where

import Control.Natural ((:~>)(..))

import           Data.Functor.Yoneda
import           Data.Functor.Coyoneda
import           Data.Functor.Kan.Ran

import           Control.Monad.Codensity
import           Control.Comonad
import           Control.Comonad.Density

import           Control.Arrow (Kleisli (..))

infixr 0 #
-- | A (natural) transformation is inside @t@, and contains @f@ and @g@
-- (typically 'Functor's).
--
-- The order of arguments allows the use of @GeneralizedNewtypeDeriving@ to wrap
-- a ':~>', but maintain the 'Transformation' constraint. Thus, @#@ can be used
-- on abstract data types.
class Transformation f g t | t -> f g where
    -- | The invocation method for a natural transformation.
    (#) :: t -> f a -> g a

-- {-# RULES "natural free theorem" [~]
--     forall h (r :: (Functor f, Functor g, Transformation f g t) => t) .
--     fmap h . (r #) = (r #) . fmap h
--   #-}

instance Transformation f g (f :~> g) where
    Nat f # g = f g


-- data Yoneda f a = Yoneda (forall b. (a -> b) -> f b)
instance Transformation ((->) a) f (Yoneda f a) where
  -- (#) :: Yoneda f a -> (a -> b) -> f b
  Yoneda y # f = y f

-- data Coyoneda f a = forall b. Coyoneda (b -> a) (f b)
instance Functor f => Transformation ((->) a) f (Coyoneda f a) where
  -- (#) :: Functor f => Coyoneda f a -> (a -> b) -> f b
  Coyoneda f fb # g = fmap (g . f) fb

-- data Kleisli g a b = Kleisli (a -> g b)
-- data Ran g h a = Ran (forall b. (a -> g b) -> h b)
instance Transformation (Kleisli g a) h (Ran g h a) where
  -- (#) :: Ran g h a -> Kleisli g a b -> h b
  Ran r # Kleisli k = r k

instance Monad m => Transformation ((->) r) m (Codensity m r) where
  Codensity c # f = c (return . f)

instance Comonad w => Transformation ((->) r) w (Density w r) where
  Density df kb # f = extend (f . df) kb

