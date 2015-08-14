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
instance Transformation ((->) x) f (Yoneda f x) where
  -- (#) :: Yoneda f x -> (x -> a) -> f a
  Yoneda y # f = y f

-- data Coyoneda f a = forall b. Coyoneda (b -> a) (f b)
instance Functor f => Transformation ((->) x) f (Coyoneda f x) where
  -- (#) :: Functor f => Coyoneda f x -> (x -> a) -> f a
  Coyoneda f fx # g = fmap (g . f) fx

-- data Kleisli g a b = Kleisli (a -> g b)
-- data Ran g h a = Ran (forall b. (a -> g b) -> h b)
instance Transformation (Kleisli g x) h (Ran g h x) where
  -- (#) :: Ran g h x -> Kleisli g x a -> h a
  Ran r # Kleisli k = r k

instance Monad m => Transformation ((->) x) m (Codensity m x) where
  -- (#) :: Monad m => Codensity m x -> (x -> a) -> m a
  Codensity c # f = c (return . f)

instance Comonad w => Transformation ((->) x) w (Density w x) where
  -- (#) :: Comonad w => Density w x -> (x -> a) -> w a
  Density df kb # f = extend (f . df) kb

