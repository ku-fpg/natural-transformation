{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeOperators #-}
{-|
Module:      Main
Copyright:   (C) 2015 The University of Kansas
License:     BSD-style (see the file LICENSE)
Maintainer:  Andy Gill
Stability:   Experimental

@QuickCheck@ properties for natural transformations.
-}
module Main (main) where

import Control.Natural ((:~>)(..))
import Control.Transformation (Transformation(..))

import Data.Foldable (toList)
#if !(MIN_VERSION_base(4,8,0))
import Data.Monoid (Monoid(..))
#endif
import Data.Sequence (Seq, fromList)

import Test.QuickCheck.Instances ()
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.QuickCheck (testProperty)

main :: IO ()
main = defaultMain testProperties

testProperties :: TestTree
testProperties = testGroup "QuickCheck properties"
    [ testProperty "Free theorem ([] :~> Seq)" (prop_freeTheorem (+1)    listSeqNT :: [Int] -> Bool)
    , testProperty "Free theorem (Seq :~> [])" (prop_freeTheorem reverse seqListNT :: Seq String -> Bool)
    , testProperty "Monoid laws"               (prop_monoidLaws listShiftNT listReverseNT listShiftNT :: [Int] -> Bool)
    ]

-- | Verifies the free theorem for natural transformations, i.e., that
-- 
-- @
-- fmap h . r == r . fmap h
-- @
prop_freeTheorem :: (Eq (g b), Functor f, Functor g, Transformation f g t)
                 => (a -> b) -> t -> f a -> Bool
prop_freeTheorem h r t = fmap h (r # t) == (r # fmap h t)

-- | Verifies that natural transformations form a law-abiding 'Monoid', i.e., that
-- 
--  * @mappend mempty x = x@
--
--  * @mappend x mempty = x@
--
--  * @mappend x (mappend y z) = mappend (mappend x y) z@
prop_monoidLaws :: (Eq (f a), Monoid t, Transformation f f t)
                => t -> t -> t -> f a -> Bool
prop_monoidLaws x y z t = (mappend mempty x # t) == (x # t)
                       && (mappend x mempty # t) == (x # t)
                       && (mappend x (mappend y z) # t)
                          == (mappend (mappend x y) z # t)

-- | A natural transformations from lists to lists that 'reverse's.
listReverseNT :: [] :~> []
listReverseNT = Nat reverse

-- | A natural transformation from lists to lists that shifts all elements to the left,
-- moving the head element to the back.
listShiftNT :: [] :~> []
listShiftNT = Nat $ \l -> case l of
                               []     -> []
                               (x:xs) -> xs ++ [x]

-- | A natural transformation from lists to 'Seq's.
listSeqNT :: [] :~> Seq
listSeqNT = Nat fromList

-- | A natural transformation from 'Seq's to lists.
seqListNT :: Seq :~> []
seqListNT = Nat toList
