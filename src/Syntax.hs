{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Syntax
-- Copyright   : (c) Adrián Enríquez Ballester, 2021
--
-- This module defines a data type for lambda terms
-- and some utilities for managing them.
module Syntax where

import qualified CodeGen  as G
import qualified Data.Set as S

-- | Data type of a lambda term where variables
-- are @'String'@s.
data Term
  -- | Variable @'Term'@ constructor
  = Var String
  -- | Abstraction @'Term'@ constructor
  | Lam String Term
   -- | Applicaton @'Term'@ constructor
  | App Term Term

-- | Show instance for a @'Term'@, avoiding
-- unnecessary parentheses.
instance Show Term where
  show (Var v) = v
  show (Lam v t) = "λ" <> v <> "." <> show t
  show (App t1 t2) =
    go True t1 <> " " <> go False t2
    where
      go _ v@(Var _)      = show v
      go True a@(App _ _) = show a
      go _ t              = "(" <> show t <> ")"

-- | Obtain a @'S.Set'@ with the variables which are
-- free in a @'Term'@.
freeVars :: Term -> S.Set String
freeVars (Var v)     = S.singleton v
freeVars (Lam v t)   = S.delete v $ freeVars t
freeVars (App t1 t2) = freeVars t1 <> freeVars t2

-- | Infix alias for @'Term'@s application.
(|>) :: Term -> Term -> Term
(|>) = App

infixl 1 |>

-- * Predefined terms

-- $
-- Variables from @'a'@ to @'z'@ and also
-- lambda abstractions from @'λa'@ to @'λz'@ are
-- provided in this module and have been
-- generated with Template Haskell.
-- This is for promote sharing and writting
-- @'Term'@s like @λx . λy $ x@

$(G.genBlocks)
