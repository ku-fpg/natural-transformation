{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}

{-|
Module:      Control.Object
Copyright:   (C) 2015 The University of Kansas
License:     BSD-style (see the file LICENSE)
Maintainer:  Andy Gill
Stability:   Experimental

An Object type, which is a natural transformation into the 'IO' monad.
-}

module Control.Object (Object(..), (#)) where

import Control.Natural

-- | An 'Object' is a natural transformation from a given 'Functor' 'f', to 'IO'.
newtype Object f = Object (f ~> IO)

instance Transformation f IO (Object f) where
    Object f # g = NT f # g
