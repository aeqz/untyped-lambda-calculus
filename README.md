# Untyped lambda calculus

An implementation of the untyped lambda calculus, both in its syntax and semantics.

Some terms and term constructors code generation is provided for writing lambda terms as the following:

```haskell
true  = λx . λy $ x
false = λx . λy $ y
```

The project tests mainly check the solutions for [these exercises](https://github.com/adrianen-ucm/formal-methods-exercises/blob/main/untyped-lambda-calculus/untyped-lambda-calculus.pdf), and some explanations are given in [this other document](https://github.com/adrianen-ucm/formal-methods-exercises/blob/main/haskell-lambda-calculus/haskell-lambda-calculus.pdf).
