module Data.Ratio

import Generics.Derive
import Category.Transaction.Qty
import Data.Nat

import JSON

%language ElabReflection

||| This is an unsafe version
||| also see https://rosettacode.org/wiki/Least_common_multiple#JavaScript

namespace QtyRatio
  public export
  record QtyRatio where
     constructor MkQr
     num : Qty
     den : Qty

  ||| for QtyRatio
  is_whole : QtyRatio -> Bool
  is_whole (MkQr a b) = if (b==1) then True else False

  fromWhole : Integer -> QtyRatio
  fromWhole x = MkQr x 1

  %runElab derive "QtyRatio" [Generic, Meta, RecordToJSON,RecordFromJSON]

  --public export
  eq_qtyratio : QtyRatio -> QtyRatio -> Bool
  eq_qtyratio (MkQr a b) (MkQr c d) = ((a*d)==(b*c))

  --public export
  add_qtyratio : QtyRatio -> QtyRatio -> QtyRatio
  add_qtyratio x@(MkQr a b) y@(MkQr c d) = if ((is_whole x)&&(is_whole y)) then (MkQr (a+c) b) else (MkQr (a*d+c*b) (b*d) )

  --public export
  abs_qtyratio : QtyRatio -> QtyRatio
  abs_qtyratio (MkQr num den) = MkQr (abs num) (abs den)


  public export
  mul_qtyratio : QtyRatio -> QtyRatio -> QtyRatio
  mul_qtyratio (MkQr a b) (MkQr c d) = (MkQr (a*c) (b*d) )

  --public export
  recip_qtyratio : QtyRatio -> QtyRatio
  recip_qtyratio (MkQr a b) = (MkQr b a)

  --public export
  div_qtyratio : QtyRatio -> QtyRatio -> QtyRatio
  div_qtyratio x y = (mul_qtyratio x (recip_qtyratio y))

  qty2int : Qty -> Int
  qty2int x = cast x

  int2qty : Int -> Qty
  int2qty x = cast x

  %foreign "C:test_gcd,libmongoose"
  test_gcd : Int -> Int -> Int

  public export
  partial 
  gcd : Qty -> Qty -> Qty
  gcd a b = if (b==0) then a else gcd b (a `mod` b)


  --%foreign "C:test_lcm,libmongoose"
  --test_lcm : Int -> Int -> Int

  --public export
  eval_qtyratio_int : QtyRatio -> QtyRatio
  eval_qtyratio_int (MkQr x y) = if ( (x==0) || (y==0)) then MkQr x y
             else
                  let xi = qty2int x
                      yi = qty2int y
                      g = test_gcd xi yi
                      xret = int2qty (div xi g) 
                      yret = int2qty (div yi g) in MkQr xret yret

  eval_qtyratio : QtyRatio -> QtyRatio
  eval_qtyratio (MkQr x y) = if ( (x==0) || (y==0)) then MkQr x y
             else
                  let g = gcd x y
                      xret = (div x g) 
                      yret = (div y g) in MkQr xret yret
  partial
  show_QtyRatio : QtyRatio -> String
  show_QtyRatio q@(MkQr n d) = 
                    let eq = eval_qtyratio q
                        xx=num eq
                        x=show $ num eq
                        y=show $ den eq
                        ret = x++"/"++y in if (xx==0) then "0" else (if is_whole eq then x else ret)

  public export
  partial
  Show QtyRatio where
      show = show_QtyRatio


  public export
  Eq QtyRatio where
     (==) = eq_qtyratio

  public export
  Ord QtyRatio where
     compare (MkQr a1 b1) (MkQr a2 b2) = 
            let a1b2 = a1*b2
                a2b1 = a2*b1 in compare a1b2 a2b1
                -- b1b2 = b1*b2 same as b2*b1

  public export
  Num QtyRatio where
     (+) x y = {-eval_qtyratio $-} add_qtyratio x y
     (*) x y = {-eval_qtyratio $-}  mul_qtyratio x y
     fromInteger = fromWhole

  public export
  Fractional QtyRatio where
     (/) x y = {-eval_qtyratio $-} div_qtyratio x y
     recip x = {-eval_qtyratio $-} recip_qtyratio x

  public export
  Cast QtyRatio Double where
     cast (MkQr num den) = if (den==0) then 0 else ((cast num)/(cast den))


  PRECISION : Double
  PRECISION = 1000

  public export
  Cast Double QtyRatio where
     cast x = (MkQr (cast $ abs (PRECISION*x)) (cast PRECISION))



namespace TQty
  public export
  data T a = Debit a | Credit a
  %runElab derive "T" [Generic, Meta, Eq, Show,ToJSON,FromJSON]     
  
  public export
  data DrCr = Dr | Cr
  %runElab derive "DrCr" [Generic, Meta, Eq, Ord,Show, EnumToJSON,EnumFromJSON]

  public export
  TQty : Type
  TQty = T QtyRatio
--  %runElab derive "TQty.TQty" [Generic, Meta, ToJSON,FromJSON]     
  
  export
  isDrCr : TQty -> DrCr    
  isDrCr (Debit x) = Dr
  isDrCr (Credit x) = Cr
  
  public export
  ToJSON TQty where
    toJSON = genToJSON' id toLower TwoElemArray

  public export
  FromJSON TQty where
    fromJSON = genFromJSON' id toLower TwoElemArray
  

  public export
  Cast TQty Double where
     cast (Debit x) = (cast x)
     cast (Credit x) = (-1.0)*(cast x)

  public export
  Cast Double TQty where
     cast x = if (x>=0.0) then Debit (cast x) else Credit (cast x)

  public export
  percent : Qty -> TQty
  percent x = if (x >= 0) then (Debit (MkQr (100-x) 100)) else (Credit (MkQr (100-x) 100))

  show_TQty : TQty -> String
  show_TQty (Debit x) = show x
  show_TQty (Credit x) = "M"++(show x)++"M"

  public export
  partial
  Show TQty where
     show = show_TQty


  add_TQty : TQty -> TQty -> TQty 
  add_TQty (Debit a) (Debit b) = Debit (a+b)
  add_TQty (Credit a) (Credit b) = Credit (a+b)
  add_TQty (Debit (MkQr a1 b1)) (Credit (MkQr a2 b2)) = 
                  let x = a1*b2 - a2*b1
                      y = b1*b2
                      ax = abs x
                      n = if (x<0) then Credit (MkQr ax y) else Debit (MkQr ax y) in n
  add_TQty (Credit (MkQr a1 b1)) (Debit (MkQr a2 b2)) = 
                  let x = a2*b1 - a1*b2
                      y = b1*b2
                      ax = abs x
                      n = if (x<0) then Credit (MkQr ax y) else Debit (MkQr ax y) in n

  mul_TQty : TQty -> TQty -> TQty
  mul_TQty (Debit x) (Debit y) = Debit (x*y)
  mul_TQty (Debit x) (Credit y) = Credit (x*y)
  mul_TQty (Credit x) (Debit y) = Credit (x*y)
  mul_TQty (Credit x) (Credit y) = Debit (x*y)

  --public export
  dr : TQty -> QtyRatio
  dr (Debit x) = x
  dr (Credit x) = 0

  --public export
  cr : TQty -> QtyRatio
  cr (Debit x) = 0
  cr (Credit x) = x

  public export
  Num TQty where
      (+) = add_TQty
      (*) = mul_TQty
      fromInteger x = if (x >= 0) then (Debit (fromWhole x)) else (Credit (fromWhole x))

  eq_TQty : TQty -> TQty -> Bool
  eq_TQty  x y = ((dr x)+(cr y)) == ((cr x)+(dr y))

  negate_TQty : TQty -> TQty
  negate_TQty (Debit x) = (Credit x) --if (x>0) then Credit (abs_qtyratio x) else Debit (abs_qtyratio x)
  negate_TQty (Credit x) = Debit x --if (x>0) then Debit (abs_qtyratio x) else Credit (abs_qtyratio x)

  sub_TQty : TQty -> TQty -> TQty
  sub_TQty x y = x + (negate_TQty y)

  recip_TQty : TQty -> TQty
  recip_TQty (Debit x) = (Debit (recip_qtyratio x))
  recip_TQty (Credit x) = (Credit (recip_qtyratio x))

  div_TQty : TQty -> TQty -> TQty
  div_TQty x y = x * (recip_TQty y)


  compare_TQty : TQty -> TQty -> Ordering
  compare_TQty (Debit x) (Debit y) = if (x==y) then EQ else compare x y
  compare_TQty (Debit x) (Credit y) = if (x==y) then EQ else GT
  compare_TQty (Credit x) (Debit y) = if (x==y) then EQ else LT
  compare_TQty (Credit x) (Credit y) = if (x==y) then EQ else compare x y


  public export
  Ord TQty where
     compare = compare_TQty

  public export
  Fractional TQty where
      (/) = div_TQty
      recip = recip_TQty

  public export
  Neg TQty where
      (-) = sub_TQty
      negate  = negate_TQty

  public export
  Eq TQty where
      (==) = eq_TQty


namespace EQty

  public export
  data EQtyType = EValue | EPercent
  %runElab derive "EQtyType" [Generic, Meta, Eq, Ord, Show, EnumToJSON,EnumFromJSON]


  public export
  data EQty : Type where
       EQVal : (et:EQtyType) -> (x:TQty) -> EQty
       EQAdd : (x:EQty) -> (y:EQty) -> EQty
       EQMul : (x:EQty) -> (y:EQty) -> EQty     
       EQNegate : (x:EQty) -> EQty
       EQSub : (x:EQty) -> (y:EQty) -> EQty
       EQDiv : (x:EQty) -> (y:EQty) -> EQty
       EQRecip : (x:EQty) -> EQty

  public export
  eval : EQty -> TQty
  eval (EQVal EValue x) = x
  eval (EQVal EPercent x) = (100-x)/100
  eval (EQAdd x y) = (eval x) + (eval y)
  eval (EQMul x y) = (eval x) * (eval y)
  eval (EQNegate x) = negate (eval x)
  eval (EQSub x y) = (eval x)-(eval y)
  eval (EQDiv x y) = (eval x)/(eval y)
  eval (EQRecip x) = recip (eval x)

  public export
  Cast Double EQty where
    cast x = EQVal EValue (cast x)

  public export
  percent : Double -> EQty
  percent x = EQVal EPercent (cast x)

  public export
  Cast EQty Double where
    cast x = cast $ eval x 

  public export
  Num EQty where
       (+) = EQAdd
       (*) = EQMul
       fromInteger x = (EQVal EValue (fromInteger x))
       
  public export
  FromDouble EQty where
       fromDouble x = (cast x)

  public export
  Neg EQty where
       (-) = EQSub
       negate = EQNegate

  public export     
  Fractional EQty where
       (/) = EQDiv
       recip = EQRecip

  public export
  Eq EQty where
       (==) x y = ( (eval x) == (eval y) )

  public export
  Ord EQty where
       compare x y = compare (eval x) (eval y)

  public export
  partial
  Show EQty where
       show = show . eval
       
  export
  isDrCr : EQty -> DrCr    
  isDrCr = (TQty.isDrCr) . eval

       

  %runElab derive "EQty" [Generic, Meta, Show, ToJSON,FromJSON]     











||| Decimal number

Precision : Type
Precision = Integer

DEFAULT_PRECISION : Precision
DEFAULT_PRECISION = 2

DECIMAL_BASE : Qty
DECIMAL_BASE = 10

getDen : Precision -> Qty
getDen n = foldl (*) 1 [DECIMAL_BASE | x <-[1..n] ]

DEFAULT_DEN : Qty
DEFAULT_DEN = (getDen DEFAULT_PRECISION)

public export
record Decimal where
  constructor MKDec
  ||| True Value, val = dec+err
  val : TQty
  
  ||| Decimal Value with denominator den
  dec : TQty
  
  ||| Error
  err : TQty  
--  ||| Denominator of num
--  den   : TQty

add_Decimal : Decimal -> Decimal -> Decimal
add_Decimal (MKDec val dec err ) (MKDec x y z) = MKDec (val+x) (dec+y) (err+z)

sub_Decimal : Decimal -> Decimal -> Decimal
sub_Decimal (MKDec val dec err ) (MKDec x y z) = MKDec (val-x) (dec-y) (err-z)

