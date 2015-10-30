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
