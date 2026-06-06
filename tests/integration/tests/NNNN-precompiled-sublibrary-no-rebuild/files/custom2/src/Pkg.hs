module Pkg
  ( pkgValue
  ) where

import Dep ( depValue )

pkgValue :: Int
pkgValue = depValue + 1
