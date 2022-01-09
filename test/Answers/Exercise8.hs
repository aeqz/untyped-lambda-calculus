-- |
-- Module      : Exercise 8
-- Copyright   : (c) Adrián Enríquez Ballester, 2021
--
-- Check the rules stated at the exercise 8.
module Answers.Exercise8 (spec) where

import           Foundation (pair, pairFst, pairSnd)
import           Semantics  (alphaEq, betaNorm)
import           Syntax     (a, b, (|>))
import           Test.Hspec (Spec, describe, it)

spec :: Spec
spec =
  describe "Exercise 8 answer" $ do

    it "fst (pair a b) is a" $
      betaNorm (pairFst |> (pair |> a |> b)) `alphaEq` a

    it "snd (pair a b) is b" $
      betaNorm (pairSnd |> (pair |> a |> b)) `alphaEq` b
