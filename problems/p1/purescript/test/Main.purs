module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)

import Test.Assert (assert)
import Main (solve)

main :: Effect Unit
main = do
  assert (solve 10 == 23)
