-- |
-- Module      : Foundation
-- Copyright   : (c) Adrián Enríquez Ballester, 2021
--
-- This module defines some well known lambda @'Term'@s.
-- It can be seen as a base library.
module Foundation
  ( iden,
    ycom,
    true,
    false,
    neg,
    conj,
    disj,
    zero,
    suc,
    pre,
    add,
    sub,
    mult,
    expo,
    pair,
    pairFst,
    pairSnd,
    nilhead,
    nil,
    conc,
    hd,
    isempty,
  )
where

import           Syntax

-- | Identity term.
iden :: Term
iden = λx x

-- | Y combinator term.
ycom :: Term
ycom = λf $ aux |> aux
  where
    aux = λx $ f |> (x |> x)

-- | Boolean true term.
true :: Term
true = λx . λy $ x

-- | Boolean false term.
false :: Term
false = λx . λy $ y

-- | Boolean negation term.
neg :: Term
neg =
  λb . λx . λy $
    b |> y |> x

-- | Booleans conjunction term.
conj :: Term
conj =
  λb . λc . λx . λy $
    b |> (c |> x |> y) |> y

-- | Booleans disjunction term.
disj :: Term
disj =
  λb . λc . λx . λy $
    b |> x |> (c |> x |> y)

-- | Natural number zero term.
zero :: Term
zero = λf . λx $ x

-- | Natural number successor term.
suc :: Term
suc =
  λn . λf . λx $
    n |> f |> (f |> x)

-- | Natural number predecessor term.
pre :: Term
pre =
  λn . λf . λx $
    n
      |> (λp . λc $ c |> (p |> (λx . λy $ y |> x)) |> f)
      |> λc (c |> x |> λx x)
      |> true

-- | Natural numbers addition term.
add :: Term
add =
  λm . λn . λf . λx $
    n |> f |> (m |> f |> x)

-- | Natural numbers subtraction term.
sub :: Term
sub =
  λm . λn $
    n |> pre |> m

-- | Natural numbers multiplication term.
mult :: Term
mult =
  λm . λn . λf $
    m |> (n |> f)

-- | Natural numbers exponentiation term.
expo :: Term
expo =
  λm . λn $
    n |> m

-- | Pair constructor term.
pair :: Term
pair =
  λa . λb . λc $
    c |> a |> b

-- | Pair first projection term.
pairFst :: Term
pairFst =
  λp $
    p |> true

-- | Pair second projection term.
pairSnd :: Term
pairSnd =
  λp $
    p |> false

-- | Variable term used to denote the error
-- of trying to get the head of @'nil'@.
nilhead :: Term
nilhead = Var "nilhead"

-- | Empty list term.
nil :: Term
nil = λf . λx $ x

-- | Lists concatenation term.
conc :: Term
conc =
  λm . λn . λf . λx $
    m |> f |> (n |> f |> x)

-- | List head term.
hd :: Term
hd = λn $ n |> true |> nilhead

-- | List empty check term.
isempty :: Term
isempty =
  λn $
    n |> (λf . λx $ false) |> true
