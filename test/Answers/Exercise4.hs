-- |
-- Module      : Exercise 4
-- Copyright   : (c) Adrián Enríquez Ballester, 2021
--
-- Check the equality stated in the exercise 4.
module Answers.Exercise4 (spec) where

import           Semantics  (alphaEq, betaNorm)
import           Syntax     (Term (App), x, y, z, λx, λy, λz, (|>))
import           Test.Hspec (Spec, describe, it)

spec :: Spec
spec =
  describe "Exercise 4 answer" $

    it "SKK = I" $
      betaNorm (App (App s k) k) `alphaEq` i

  where
    s = λx . λy . λz $ (x |> z) |> (y |> z)
    k = λx . λy $ x
    i = λx x
