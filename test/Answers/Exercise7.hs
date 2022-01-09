-- |
-- Module      : Exercise 7
-- Copyright   : (c) Adrián Enríquez Ballester, 2021
--
-- Check the examples given at the exercise 7.
module Answers.Exercise7 (spec) where

import           Foundation (add, expo, mult, suc, zero)
import           Semantics  (alphaEq, betaNorm)
import           Syntax     ((|>))
import           Test.Hspec (Spec, describe, it)

spec :: Spec
spec =
  describe "Exercise 7 answer" $ do

    it "add 3 2 is 5" $
      betaNorm (add |> three |> two) `alphaEq` betaNorm five

    it "mult 3 2 is 6" $
      betaNorm (mult |> three |> two) `alphaEq` betaNorm six

    it "expo 3 2 is 9" $
      betaNorm (expo |> three |> two) `alphaEq` betaNorm nine

  where
    one = suc |> zero
    two = suc |> one
    three = suc |> two
    four = suc |> three
    five = suc |> four
    six = suc |> five
    seven = suc |> six
    eight = suc |> seven
    nine = suc |> eight
