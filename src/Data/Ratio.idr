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

public export
percent : Qty -> QtyRatio
percent x = (MkQr (100-x) 100)


public export
Show QtyRatio where
    show (MkQr num den) = if (den==1) then (show num) else (show num)++"/"++(show den)


%runElab derive "QtyRatio" [Generic, Meta, RecordToJSON,RecordFromJSON]

is_whole : QtyRatio -> Bool
is_whole (MkQr a b) = if (b==1) then True else False

--public export
eq_qtyratio : QtyRatio -> QtyRatio -> Bool
eq_qtyratio (MkQr a b) (MkQr c d) = ((a*d)==(b*c))

--public export
add_qtyratio : QtyRatio -> QtyRatio -> QtyRatio
add_qtyratio x@(MkQr a b) y@(MkQr c d) = if ((is_whole x)&&(is_whole y)) then (MkQr (a+c) b) else (MkQr (a*d+c*b) (b*d) )


--public export
sub_qtyratio : QtyRatio -> QtyRatio -> QtyRatio
sub_qtyratio x@(MkQr a b) y@(MkQr c d) = if ((is_whole x)&&(is_whole y)) then (MkQr (a-c) b) else (MkQr (a*d - c*b) (b*d) )


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
Eq QtyRatio where
   (==) = eq_qtyratio
   (/=) x y = (not (x==y))
   
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
   (+) x y = eval_qtyratio $ add_qtyratio x y
   (*) x y = eval_qtyratio $  mul_qtyratio x y
   fromInteger x = MkQr x 1

public export
Neg QtyRatio where
   (-) x y = eval_qtyratio $ sub_qtyratio x y
   negate (MkQr x y) = eval_qtyratio (MkQr ((-1)*x) y )
   
public export
Fractional QtyRatio where
   (/) x y = eval_qtyratio $ div_qtyratio x y
   recip x = eval_qtyratio $ recip_qtyratio x

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
