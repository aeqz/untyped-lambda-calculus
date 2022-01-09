-- |
-- Module      : Exercise 2
-- Copyright   : (c) Adrián Enríquez Ballester, 2021
--
-- Check the answer of the exercise 2.
module Answers.Exercise2 (spec) where

import           Semantics  (alphaEq, betaNorm)
import           Syntax     (x, y, z, λx, λy, λz, (|>))
import           Test.Hspec (Spec, describe, it)

spec :: Spec
spec =
  describe "Exercise 2 answer" $

    it (show b1 <> " beta normalizes to " <> show b2) $
      betaNorm b1 `alphaEq` b2

  where
    b1 = (λx . λy $ x |> y) |> y
    b2 = λz $ y |> z
