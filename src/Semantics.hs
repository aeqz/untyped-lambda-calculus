-- |
-- Module      : Semantics
-- Copyright   : (c) Adrián Enríquez Ballester, 2021
--
-- Semantic definitions for lambda @'Term'@s.
module Semantics
  ( subs,
    alphaEq,
    betaRed,
    etaRed,
    normalOrder,
    betaNorm,
    betaEtaNorm,
  )
where

import           Control.Applicative (Alternative, (<|>))
import qualified Data.Set            as S
import           Syntax              (Term (..), freeVars)

-- | @subs v e t@ replaces the free occurrences
-- of @v@ in @t@ by @e@, checking and avoiding
-- variable capture to happen.
subs :: String -> Term -> Term -> Term
subs v e =
  go . filter (`S.notMember` freeE) . auxVars $ 'x'
  where
    freeE = freeVars e
    go aux (App t1 t2) = App (go aux t1) (go aux t2)
    go _ t@(Var v')
      | v' == v = e
      | otherwise = t
    go aux t@(Lam v' b)
      | v' == v || not bodyCapture = t
      | bodyCapture && not varCapture = Lam v' $ go aux b
      | otherwise = Lam auxVar . go aux' . subs v' (Var auxVar) $ b
      where
        freeT = freeVars b
        bodyCapture = v `S.member` freeT
        varCapture = v' `S.member` freeE
        (auxVar : aux') = dropWhile (`S.member` freeT) aux

-- | Simple generator of variable names given
-- a character prefix: @x0, x1, x2, ...@
auxVars :: Char -> [String]
auxVars p = map ((p :) . show) [0 :: Integer ..]

-- | Check if two @'Term'@s are alpha-equivalent
-- (i.e. equal modulo bound variable names).
alphaEq :: Term -> Term -> Bool
alphaEq (Var x) (Var y) = x == y
alphaEq (Lam x t1) (Lam y t2)
  | x == y = t1 `alphaEq` t2
  | x `S.member` freeVars t2 = False
  | otherwise = t1 `alphaEq` subs y (Var x) t2
alphaEq (App t11 t12) (App t21 t22) =
  t11 `alphaEq` t21 && t12 `alphaEq` t22
alphaEq _ _ = False

-- | Perform a beta-reduction only if the
-- supplied @'Term'@ is a beta-redex.
betaRed :: Term -> Maybe Term
betaRed (App (Lam v t) e) = Just $ subs v e t
betaRed _                 = Nothing

-- | Perform an eta-reduction only if the
-- supplied @'Term'@ is an eta-redex.
etaRed :: Term -> Maybe Term
etaRed (Lam v (App t (Var v')))
  | v == v' && v `S.notMember` freeVars t = Just t
etaRed _ = Nothing

-- | Perform a supplied @'Term'@ transformation
-- which can fail (e.g. @'betaRed'@ and @'etaRed'@)
-- in normal order (i.e. to the leftmost-outermost
-- subexpression which admits such transformation).
normalOrder :: Alternative f => (Term -> f Term) -> Term -> f Term
normalOrder red v@(Var _) = red v
normalOrder red l@(Lam v t) =
  red l
    <|> (Lam v <$> normalOrder red t)
normalOrder red a@(App t1 t2) =
  red a
    <|> ((`App` t2) <$> normalOrder red t1)
    <|> ((t1 `App`) <$> normalOrder red t2)

-- | Reduce a @'Term'@ into beta-normal form
-- if it is normalizable. Otherwise, its
-- computation will not terminate.
betaNorm :: Term -> Term
betaNorm t = maybe t betaNorm $ beta t
  where
    beta = normalOrder betaRed

-- | Reduce a @'Term'@ into beta-eta-normal form
-- if it is normalizable. Otherwise, its
-- computation will not terminate.
betaEtaNorm :: Term -> Term
betaEtaNorm t = maybe t betaEtaNorm $ beta t <|> eta t
  where
    beta = normalOrder betaRed
    eta = normalOrder etaRed
