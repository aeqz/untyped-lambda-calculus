-- |
-- Module      : Main
-- Copyright   : (c) Adrián Enríquez Ballester, 2021
--
-- Test suite for the untyped lambda calculus implementation.
--
-- It consists of checking some answers, equations and solutions
-- from the exercises.
module Main (main) where

import qualified Answers.Exercise1  as E1
import qualified Answers.Exercise10 as E10
import qualified Answers.Exercise2  as E2
import qualified Answers.Exercise4  as E4
import qualified Answers.Exercise6  as E6
import qualified Answers.Exercise7  as E7
import qualified Answers.Exercise8  as E8
import qualified Answers.Exercise9  as E9
import qualified Termination        as T
import           Test.Hspec         (hspec)

main :: IO ()
main = hspec $ do
  E1.spec
  E2.spec
  E4.spec
  E6.spec
  E7.spec
  E8.spec
  E9.spec
  E10.spec
  T.spec
