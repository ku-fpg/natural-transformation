# natural-transformation [![Hackage version](https://img.shields.io/hackage/v/natural-transformation.svg?style=flat)](http://hackage.haskell.org/package/natural-transformation) [![Build Status](https://github.com/ku-fpg/natural-transformation/workflows/Haskell-CI/badge.svg)](https://github.com/ku-fpg/natural-transformation/actions?query=workflow%3AHaskell-CI)

A natural transformation transforms a container `f a` into another container `g a`.
Natural transformations act as functor morphisms in category theory.
Technically, `f` and `g` should be functors, but we allow any correctly-shaped structure.
