-- |
-- Module      : Exercise 6
-- Copyright   : (c) Adrián Enríquez Ballester, 2021
--
-- Check the answers of the exercise 6.
module Answers.Exercise6 (spec) where

import           Foundation (conj, disj, false, neg, true)
import           Semantics  (alphaEq, betaEtaNorm, betaNorm)
import           Syntax     (b, (|>))
import           Test.Hspec (Spec, describe, it)

spec :: Spec
spec =
  describe "Exercise 6 answer" $ do

    it "neg true is false" $
      betaNorm (neg |> true) `alphaEq` false

    it "neg false is true" $
      betaNorm (neg |> false) `alphaEq` true

    it "conj false b is false" $
      betaNorm (conj |> false |> b) `alphaEq` false

    it "conj true b is b" $
      betaEtaNorm (conj |> true |> b) `alphaEq` b

    it "disj true b is true" $
      betaNorm (disj |> true |> b) `alphaEq` true

    it "disj false b is b" $
      betaEtaNorm (disj |> false |> b) `alphaEq` b
