-- |
-- Module      : Exercise 10
-- Copyright   : (c) Adrián Enríquez Ballester, 2021
--
-- Check the examples given at the exercise 10.
module Answers.Exercise10 (spec) where

import           Foundation (conc, false, hd, isempty, nil, nilhead, true)
import           Semantics  (alphaEq, betaNorm)
import           Syntax     (a, b, c, d, f, x, λf, λx, (|>))
import           Test.Hspec (Spec, describe, it)

spec :: Spec
spec =
  describe "Exercise 10 answer" $ do

    it ("app (" <> show l1 <> ") (" <> show l2 <> ") is " <> show l3) $
      betaNorm (conc |> l1 |> l2) `alphaEq` l3

    it ("hd (" <> show nil <> ") is " <> show nilhead) $
      betaNorm (hd |> nil) `alphaEq` nilhead

    it ("hd (" <> show l1 <> ") is " <> show a) $
      betaNorm (hd |> l1) `alphaEq` a

    it ("isempty (" <> show nil <> ") is " <> show true) $
      betaNorm (isempty |> nil) `alphaEq` true

    it ("isempty (" <> show l2 <> ") is " <> show false) $
      betaNorm (isempty |> l2) `alphaEq` false

  where
    l1 = λf . λx $ f |> a |> (f |> b |> x)
    l2 = λf . λx $ f |> c |> (f |> d |> x)
    l3 = λf . λx $ f |> a |> (f |> b |> (f |> c |> (f |> d |> x)))
