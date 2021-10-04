module Rational.NonZeroQty

import Category.Transaction.Qty

record NonZeroQty where
  constructor MkNonZeroQty
  num : Qty
  nonZero : Not (num = 0)
  

--postulate

{-
zeroProductProperty : (a, b : Qty) -> a * b = 0 -> Either (a = 0) (b = 0)

productOfNonZeroIsNonZero : NonZeroQty -> NonZeroQty -> NonZeroQty
productOfNonZeroIsNonZero (MkNonZeroQty num1 nonZero1) (MkNonZeroQty num2 nonZero2) = MkNonZeroQty (num1 * num2) prf
  where
    prf = \prodIsZero => case zeroProductProperty num1 num2 prodIsZero of
      Left  aIsZero => nonZero1 aIsZero
      Right bIsZero => nonZero2 bIsZero
-}
