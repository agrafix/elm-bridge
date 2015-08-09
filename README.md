Elm Bridge
=====

[![Build Status](https://travis-ci.org/agrafix/elm-bridge.svg)](https://travis-ci.org/agrafix/elm-bridge)

[![Hackage Deps](https://img.shields.io/hackage-deps/v/elm-bridge.svg)](http://packdeps.haskellers.com/reverse/elm-bridge)

## Intro

Hackage: [elm-bridge](http://hackage.haskell.org/package/elm-bridge)

 **WARNING: Work in progress!**

Building the bridge from [Haskell](http://haskell.org) to [Elm](http://elm-lang.org) and back. Define types once, use on both sides and enjoy easy (de)serialisation. Cheers!

## Usage

```haskell
{-# LANGUAGE TemplateHaskell #-}
import Elm.Derive
import Elm.TyRender
import Elm.TyRep

import Data.Proxy

data Foo
   = Foo
   { f_name :: String
   , f_blablub :: Int
   } deriving (Show, Eq)

main :: IO ()
main =
    putStrLn $ renderElm $ compileElmDef (Proxy :: Proxy Foo)
```

Output will be:

```elm
type alias Foo =
	{ f_name: String
	, f_blablub: Int
	}
```

For more usage examples check the tests.

## Install

* Using cabal: `cabal install elm-bridge`
* From Source: `git clone https://github.com/agrafix/elm-bridge.git && cd elm-bridge && cabal install`

## Todo

* Generate Elm JSON Serializer/Parser