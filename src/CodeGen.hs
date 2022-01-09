-- |
-- Module      : CodeGen
-- Copyright   : (c) Adrián Enríquez Ballester, 2021
--
-- This module defines a Template Haskell method to
-- generate some repetitive @'Term'@ definitions.
module CodeGen (genBlocks) where

import qualified Language.Haskell.TH as TH

genBlocks :: TH.Q [TH.Dec]
genBlocks = do
  termType <- TH.lookupTypeName "Term"
  varCons <- TH.lookupValueName "Var"
  lamCons <- TH.lookupValueName "Lam"
  maybe
    (fail "Syntax not in scope")
    (pure . reverse . mconcat . (<$> ['a'..'z']))
    (block <$> termType <*> varCons <*> lamCons)

block :: TH.Name -> TH.Name -> TH.Name -> Char -> [TH.Dec]
block termType varCons lamCons x =
  [ TH.SigD varName $
      TH.ConT termType,
    TH.FunD
      varName
      [TH.Clause [] (TH.NormalB (TH.AppE (TH.ConE varCons) nameLit)) []],
    TH.SigD lamName $
      TH.AppT (TH.AppT TH.ArrowT (TH.ConT termType)) (TH.ConT termType),
    TH.FunD
      lamName
      [TH.Clause [] (TH.NormalB (TH.AppE (TH.ConE lamCons) nameLit)) []]
  ]
  where
      name = x : ""
      varName = TH.mkName name
      lamName = TH.mkName $ 'λ' : name
      nameLit = TH.LitE . TH.StringL $ name
