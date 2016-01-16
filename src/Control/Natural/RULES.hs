{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module:      Control.Transformation.RULES
Copyright:   (C) 2015 The University of Kansas
License:     BSD-style (see the file LICENSE)
Maintainer:  Andy Gill
Stability:   Experimental

A type class for transformations.
-}
module Control.Natural.RULES () where

import Control.Natural

{-# RULES "natural free theorem" [~]
     forall h (r :: (Functor f, Functor g, Transformation f g t) => t) .
       fmap h . (r #) = (r #) . fmap h
  #-}

