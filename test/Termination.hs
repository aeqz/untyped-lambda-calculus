-- |
-- Module      : Termination
-- Copyright   : (c) Adrián Enríquez Ballester, 2021
--
-- Test if a reduction which may not terminate in
-- other reduction order terminates in normal order.
module Termination (spec) where

import           Data.Maybe (isNothing)
import           Foundation (iden, true)
import           Semantics  (betaEtaNorm, betaNorm, betaRed, normalOrder)
import           Syntax     (x, λx, (|>))
import           Test.Hspec (Spec, describe, it)

spec :: Spec
spec =
  describe "Termination" $ do

    it (show t <> " normal order beta-reduction should terminate") $
      isNothing $ normalOrder betaRed $ betaNorm t

    it (show t <> " normal order beta-eta-reduction should terminate") $
      isNothing $ normalOrder betaRed $ betaEtaNorm t

  where
    t = true |> iden |> (self |> self)
    self = λx $ x |> x
