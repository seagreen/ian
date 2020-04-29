module SExpression where

-- http://www-formal.stanford.edu/jmc/recursive.pdf
-- Recursive Functions of Symbolic Expressions and Their Computation by Machine, Part I
-- John McCarthy, MIT, 1960
--
--  We shall now define the S-expressions
-- (S stands for symbolic). They are formed by using the special characters
--
--     (
--     .
--     )
--
-- and an infinite set of distinguishable atomic symbols.
--
-- S-expressions are then defined as follows:
-- 1. Atomic symbols are S-expressions.
-- 2. If e1 and e2 are S-expressions, so is (e1 · e2).
--
-- NIL is an atomic symbol used to terminate lists.
--
-- Since many of the
-- symbolic expressions with which we deal are conveniently expressed as lists,
-- we shall introduce a list notation to abbreviate certain S-expressions. We have
--
-- 1. (m) stands for (m · NIL).
-- 2. (m1, · · · , mn) stands for (m1 · (· · ·(mn · NIL)· · ·)).
-- 3. (m1, · · · , mn · x) stands for (m1 · (· · ·(mn · x)· · ·)).

-- (a b) is sugar for (a . (b . NIL))
-- there's no list sugar for (a . b)

-- From the s-cargot package:
-- https://hackage.haskell.org/package/s-cargot-0.1.4.0/docs/Data-SCargot-Repr.html
data SExpr atom
  = SCons (SExpr atom) (SExpr atom)
  | SAtom atom
  | SNil

-- Proper lists are ones that end in a null identifier.

-- At read time you can make a list by doing:
-- '(1 2 3 4 5)
--
-- But at run time you have to use a function:
-- (list a b c d e)

data SExprHighLevel atom
  = HLList [SExprHighLevel atom]
  | HLNil
