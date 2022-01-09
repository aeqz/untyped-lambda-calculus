-- |
-- Module      : Exercise 1
-- Copyright   : (c) Adrián Enríquez Ballester, 2021
--
-- Check the answer of the exercise 1.
module Answers.Exercise1 (spec) where

import           Data.Foldable (forM_)
import           Semantics     (alphaEq)
import           Syntax        (f, w, x, y, z, λf, λw, λx, λy, λz, (|>))
import           Test.Hspec    (Spec, describe, it)

spec :: Spec
spec = do
  describe "Exercise 1 answer" $ do

    forM_
      [ (t1, t2)
      | c <- [c1, c2, c3, c4]
      , t1 <- c
      , t2 <- c
      ] $ \(t1, t2) ->
        it (show t1 <> " is aplha equivalent to " <> show t2)
          (t1 `alphaEq` t2)

    forM_
      [ (t1, t2)
      | (c, cs) <- choices [c1, c2, c3, c4]
      , c' <- cs
      , t1 <- c
      , t2 <- c'
      ] $ \(t1, t2) ->
        it (show t1 <> " is not aplha equivalent to " <> show t2)
          (not $ t1 `alphaEq` t2)

    it (show b1 <> " is aplha equivalent to " <> show b2) $
      b1 `alphaEq` b2

  where
    b1 = λx $ x
      |> λy (x |> y)
      |> λx x
      |> λy (y |> x)
    b2 = λx $ x
      |> λy (x |> y)
      |> λz z
      |> λw (w |> x)
    c1 =
      [ λx $ x |> y
      , λz $ z |> y
      , λf $ f |> y
      ]
    c2 =
      [ λx $ x |> z
      , λy $ y |> z
      ]
    c3 =
      [ λz $ z |> z
      , λf $ f |> f
      ]
    c4 =
      [ λy . λx $ x |> y
      , λz . λy $ y |> z
      ]

choices :: [a] -> [(a, [a])]
choices []       = []
choices (a : as) =
  (a, as) : (fmap (a :) <$> choices as)
