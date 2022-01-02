module Examples.Performance2

import Data.DPair
import Data.Either
import Data.List.TR
import Data.Nat
import Data.String
import Examples.CSS.Performance
import Examples.Util
import Generics.Derive
import Rhone.JS

%language ElabReflection
%default total
data Ev = Reload | Validate

%runElab derive "Ev" [Generic,Meta,Show,Eq]

PosNat : Type
PosNat = Subset Nat IsSucc

validate : String -> Either String PosNat
validate s = case cast {to = Nat} s of
  Z       => Left "Not a positive natural number: \{s}"
  n@(S _) => Right $ Element n ItIsSucc

btnRef : Nat -> ElemRef Button
btnRef n = Id Button "BTN\{show n}"

btn : Nat -> Node Nat
btn n =
  button
    [ref (btnRef n), onClick n, classes [widget,btn,inc]]
    [Text $ show n]

btns : PosNat -> Node Nat
btns (Element n _) = div [class grid] . mapTR btn $ iterateTR n (+1) 1

content : Node Ev
content =
  div [ class performanceContent ]
    [ lbl "Number of buttons:" numButtonsLbl
    , input [ ref natIn
            , onInput (const Validate)
            , onEnterDown Reload
            , classes [widget, textIn]
            , placeholder "Enter a positive integer"
            ] []
    , button [ref btnRun, onClick Reload, classes [widget, btn]] ["Run"]
    , lbl "Sum:" sumLbl
    , div [ref out] []
    , div [ref time] []
    , div [ref buttons] []
    ]

public export
MI : Type -> Type
MI = DomIO Ev JSIO

public export
MB : Type -> Type
MB = DomIO Nat JSIO

dispTime : Nat -> Integer -> String
dispTime 1 ms = "\Loaded one button in \{show ms} ms."
dispTime n ms = "\Loaded \{show n} buttons in \{show ms} ms."

sumNats : MSF MB Nat ()
sumNats = fan_
  [ accumulateWith (+) 0 >>> show ^>> text out
  , ifIsNot 0 $ fan [arr btnRef, const True] >>> disabled
  ]

btnsSF : PosNat -> MB (MSF MB Nat (), JSIO ())
btnsSF n = do
  t1 <- currentTime
  innerHtmlAt buttons (btns n)
  t2 <- currentTime
  rawInnerHtmlAt time (dispTime n.fst $ t2 - t1)
  pure (sumNats, pure ())

count : MSF MI Ev (Either String PosNat)
count =    getInput Validate validate natIn
       >>> observeWith (isLeft ^>> disabledAt btnRun)

msf : MSF MI Ev ()
msf =   fan [count, Data.MSF.Util.is Reload]
    >>> rightOnEvent
    ?>> arrM (ignore . reactimateInDomIni 0 . btnsSF)

export
ui : MI (MSF MI Ev (), JSIO ())
ui = innerHtmlAt exampleDiv content $> (msf, pure ())
