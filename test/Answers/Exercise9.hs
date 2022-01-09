-- |
-- Module      : Exercise 9
-- Copyright   : (c) Adrián Enríquez Ballester, 2021
--
-- Check the examples given at the exercise 9.
module Answers.Exercise9 (spec) where

import           Foundation (pre, sub, suc, zero)
import           Semantics  (alphaEq, betaNorm)
import           Syntax     ((|>))
import           Test.Hspec (Spec, describe, it)

spec :: Spec
spec =
  describe "Exercise 9 answer" $ do

    it "pred 3 is 2" $
      betaNorm (pre |> three) `alphaEq` betaNorm two

    it "sub 3 2 is 1" $
      betaNorm (sub |> three |> two) `alphaEq` betaNorm one

  where
    one = suc |> zero
    two = suc |> one
    three = suc |> two
