module Data.Ratio

import Generics.Derive
import Category.Transaction.Qty
import Data.Nat

import JSON

%language ElabReflection

||| This is an unsafe version
||| also see https://rosettacode.org/wiki/Least_common_multiple#JavaScript


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


--public export
sub_qtyratio : QtyRatio -> QtyRatio -> QtyRatio
sub_qtyratio x@(MkQr a b) y@(MkQr c d) = if ((is_whole x)&&(is_whole y)) && (b==d) then (MkQr (a-c) b) 
                             else let ad=a*d
                                      cb=c*b
                                      ret = (MkQr (ad-cb) (b*d) ) in ret



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

%foreign "C:test_lcm,libmongoose"
test_lcm : Int -> Int -> Int

--public export
eval_qtyratio : QtyRatio -> QtyRatio
eval_qtyratio (MkQr x y) = if ( (x==0) || (y==0)) then MkQr x y
           else
                let xi = qty2int x
                    yi = qty2int y
                    g = test_gcd xi yi
                    xret = int2qty (div xi g) 
                    yret = int2qty (div yi g) in MkQr xret yret
public export
Show QtyRatio where
    show q@(MkQr n d) = 
                  let eq = eval_qtyratio q
                      x=show $ num eq
                      y=show $ den eq
                      ret = x++"/"++y in if is_whole eq then (show x)  else ret


public export
Eq QtyRatio where
   (==) = eq_qtyratio
   --(/=) x y = (not (x==y))
   
public export
Ord QtyRatio where
   compare (MkQr a b) (MkQr c d) = if ( ((b==1) && (d==1)) || ((b==0) || (d==0)) )then (compare a b)     
           else
                let xa = qty2int a
                    xc = qty2int c
                    xb = qty2int b
                    xd = qty2int d                    
                    bd_lcm = test_lcm xb xd
                    
                    na = (div bd_lcm xa)
                    nc = (div bd_lcm xc)
                    in if (na==nc) then EQ
                          else if (na>nc) then LT else GT


public export
Num QtyRatio where
   (+) x y = {-eval_qtyratio $-} add_qtyratio x y
   (*) x y = {-eval_qtyratio $-}  mul_qtyratio x y
   fromInteger = fromWhole

public export
Neg QtyRatio where
   (-) x y = {-eval_qtyratio $-} sub_qtyratio x y
   negate (MkQr x y) = {-eval_qtyratio-} (MkQr ((-1)*x) y )
   
public export
Fractional QtyRatio where
   (/) x y = {-eval_qtyratio $-} div_qtyratio x y
   recip x = {-eval_qtyratio $-} recip_qtyratio x

public export
d1 : Double
d1 = 43.32

--Eq, Ord

public export
r1 : QtyRatio
r1 = (MkQr 7 4)

public export
r2 : QtyRatio
r2 = (MkQr 2 3)

public export
r7 : QtyRatio
r7 = (MkQr 7 1)

public export
r4 : QtyRatio
r4 = (MkQr 4 1)

public export
r0 : QtyRatio
r0 = (MkQr 1 0)


||| For TQty
public export
data T a = Debit a | Credit a


public export
TQty : Type
TQty = T QtyRatio

public export
percent : Qty -> TQty
percent x = if (x >= 0) then (Debit (MkQr (100-x) 100)) else (Credit (MkQr (100-x) 100))

show_TQty : TQty -> String
show_TQty (Debit x) = show x
show_TQty (Credit x) = "M"++(show x)++"M"

public export
Show TQty where
   show = show_TQty

%runElab derive "T" [Generic, Meta, Eq, Show,ToJSON,FromJSON]     

add_TQty : TQty -> TQty -> TQty 
add_TQty (Debit a) (Debit b) = Debit (a+b)
add_TQty (Credit a) (Credit b) = Credit (a+b)
add_TQty (Debit a) (Credit b) = if (a<b) then Credit (b-a)
                                  else if (a>b) then Debit (a-b) 
                                       else Debit 0
add_TQty (Credit a) (Debit b) = if (a<b) then Debit (b-a)
                                  else if (a>b) then Credit (a-b) 
                                       else Debit 0


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
negate_TQty (Debit x) = if (x>0) then Credit (abs_qtyratio x) else Debit (abs_qtyratio x)
negate_TQty (Credit x) = if (x>0) then Debit (abs_qtyratio x) else Credit (abs_qtyratio x)

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
    

public export
data EQty : Type where
     EQVal : (x:TQty) -> EQty
     EQAdd : (x:EQty) -> (y:EQty) -> EQty
     EQNegate : (x:EQty) -> EQty
     EQSub : (x:EQty) -> (y:EQty) -> EQty
     EQDiv : (x:EQty) -> (y:EQty) -> EQty
     EQRecip : (x:EQty) -> EQty

public export
eval : EQty -> TQty
eval (EQVal x) = x
eval (EQAdd x y) = (eval x) + (eval y)
eval (EQNegate x) = negate (eval x)
eval (EQSub x y) = (eval x)-(eval y)
eval (EQDiv x y) = (eval x)/(eval y)
eval (EQRecip x) = recip (eval x)

public export
Num EQty where
     (+) = EQAdd
     (*) = EQDiv
     fromInteger x = (EQVal (fromInteger x))

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

