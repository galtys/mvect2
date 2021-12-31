module Examples.MathGame2

import Control.Monad.State
import Data.List
import Data.MSF.Trans
import Data.Vect
import Examples.CSS.MathGame
import Examples.Util
import Generics.Derive
import Rhone.Canvas
import Rhone.JS
import System.Random
import Text.CSS

%language ElabReflection
%default total
data Language = EN | DE

%runElab derive "Language" [Generic,Meta,Show,Eq]

public export
data Ev = Lang String | Check | NewGame

%runElab derive "Ev" [Generic,Meta,Show,Eq]
data Op = Plus | Minus | Mult

record Calc where
  constructor MkCalc
  x  : Integer
  y  : Integer
  op : Op

result : Calc -> Integer
result (MkCalc x y Plus)  = x + y
result (MkCalc x y Minus) = x - y
result (MkCalc x y Mult)  = x * y

dispCalc : Calc -> String
dispCalc (MkCalc x y op) = "\{show x} \{dispOp op} \{show y} = "
  where dispOp : Op -> String
        dispOp Plus  = "+"
        dispOp Minus = "-"
        dispOp Mult  = "*"
record Tile where
  constructor MkTile
  posX    : Bits8
  posY    : Bits8
  calc    : Calc

record GameState where
  constructor MkGS
  lang   : Language
  rows   : Bits8
  wrong  : List Tile
  calcs  : List Tile
  pic    : String

currentCalc : GameState -> Maybe Calc
currentCalc gs = case gs.calcs of
  t :: _ => Just t.calc
  Nil    => Nothing
pictures : List String
pictures = map (\n => "pics/pic\{show n}.jpg") [the Bits8 1..11]
data Result = Ended Language
            | Correct Language
            | Wrong Language Calc Integer

style : Result -> Maybe String
style (Ended _)     = Nothing
style (Correct _)   = Just "color : \{render green}"
style (Wrong _ _ _) = Just "color : \{render red}"

language : Language -> String
language DE = "Sprache"
language EN = "Language"

german : Language -> String
german DE = "Deutsch"
german EN = "German"

english : Language -> String
english DE = "Englisch"
english EN = "English"

resultStr : Language -> String
resultStr DE = "Resultat"
resultStr EN = "result"

checkAnswerStr : Language -> String
checkAnswerStr DE = "Antwort prüfen"
checkAnswerStr EN = "Check answer"

newGameStr : Language -> String
newGameStr DE = "Neues Spiel"
newGameStr EN = "New game"

reply : Result -> String
reply (Ended EN)   = "The game has ended."
reply (Correct EN) = "Correct!"
reply (Ended DE)   = "Das Spiel ist vorbei."
reply (Correct DE) = "Richtig!"
reply (Wrong EN c n) =
     "That's not correct. Your answer was \{show n}. "
  ++ "The correct answer is: \{dispCalc c} \{show $ result c}."
reply (Wrong DE c n) =
     "Leider falsch. Deine Antwort war \{show n}. "
  ++ "Die richtige Antwort ist: \{dispCalc c} \{show $ result c}."
wcanvas : Bits32
wcanvas = 500

content : Language -> Node Ev
content l =
  div [ class mathContent ]
    [ lbl "\{language l}:" lblLang
    , select
        [ ref langIn, classes [widget, selectIn], onChange Lang]
        [ option [ value "de", selected (l == DE)] [Text $ german l]
        , option [ value "en", selected (l == EN)] [Text $ english l]
        ]

    , div [ ref calc ] []

    , input [ ref resultIn
            , onEnterDown Check
            , class widget
            , placeholder (resultStr l)
            ] []

    , button [ ref checkBtn
             , onClick Check
             , classes [widget,btn]
             ] [Text $ checkAnswerStr l]

    , button [ ref newBtn
             , onClick NewGame
             , classes [widget,btn]
             ] [Text $ newGameStr l]

    , div [ ref out ] []

    , canvas [ ref pic, width wcanvas, height wcanvas ] []
    ]
tile : Tile -> Scene
tile t = S1 [] Id $ Rect (cast t.posX) (cast t.posY) 1 1 Fill

stuckColor : Color
stuckColor = HSLA 0 0 50 80

dispState : GameState -> Scene
dispState gs =
  let sf = cast {to = Double} wcanvas / cast gs.rows
   in SM [] (scale sf sf)
        [ SM [ Fill black ] Id $ map tile gs.calcs
        , SM [ Fill stuckColor ] Id $ map tile gs.wrong
        ]

renderGame : LiftJSIO m => GameState -> m ()
renderGame gs =
  render $ MkCanvas pic (cast wcanvas) (cast wcanvas) (dispState gs)
upperBound : Int32
upperBound = 100

randomCalc : HasIO io => io Calc
randomCalc = do
  op   <- rndSelect' [Plus,Minus,Mult]
  case op of
    Plus => do
      x <- randomRIO (0, upperBound)
      y <- randomRIO (0, upperBound - x)
      pure $ MkCalc (cast x) (cast y) op

    Minus => do
      x <- randomRIO (0, upperBound)
      y <- randomRIO (0, x)
      pure $ MkCalc (cast x) (cast y) op

    Mult => do
      x <- randomRIO (1, 12)
      y <- randomRIO (0, upperBound `div` x)
      pure $ MkCalc (cast x) (cast y) op

randomTile : HasIO io => (Bits8,Bits8) -> io (Int32, Tile)
randomTile (px,py) = do
  c       <- randomCalc
  sortVal <- randomRIO (0, 1000)
  pure (sortVal, MkTile px py c)

randomGame : HasIO io => Language -> io GameState
randomGame l = do
  pic   <- rndSelect pictures
  pairs <- traverse randomTile [| MkPair [0..3] [0..3] |]
  let ts = snd <$> sortBy (comparing fst) pairs
  pure $ MkGS l 4 Nil ts pic
checkAnswer : String -> GameState -> NP I [Result,GameState]
checkAnswer s (MkGS l nr wrong (h :: t) pic) =
  let answer = cast s
   in if result h.calc == answer
      then [Correct l, MkGS l nr wrong t pic]
      else [Wrong l h.calc answer, MkGS l nr (h :: wrong) t pic]
checkAnswer s gs = [Ended gs.lang, gs]

setPic : LiftJSIO m => MSF m GameState ()
setPic =   (\gs => "background-image : url('\{gs.pic}');")
       ^>> attributeAt_ "style" pic

dispGame : LiftJSIO m => MSF m GameState ()
dispGame = fan_ [ currentCalc ^>-
                    [ isNothing ^>- [disabledAt checkBtn, disabledAt resultIn]
                    , maybe "" dispCalc ^>> text calc
                    ]
                , arrM renderGame
                , const "" >>> Sink.valueOf resultIn
                , setPic
                ]
check : MonadState GameState m => LiftJSIO m => MSF m i ()
check =  [| checkAnswer (valueOf resultIn) get |]
      >>- [ snd >>! put
          , hd  >>> reply ^>> text out
          , hd  >>> style ^>> attributeAt "style" out
          ]
public export
M : Type -> Type
M = DomIO Ev JSIO

newGame : LiftJSIO m => MSF (StateT GameState m) i ()
newGame = get >>> lang ^>> randomGame !>- [setPic, put]

adjLang : MSF (StateT GameState M) Ev ()
adjLang = readLang ^>> ifJust (
            arrM $ \l => innerHtmlAt exampleDiv (content l)
                      >> modify (record { lang = l })
          )
  where readLang : Ev -> Maybe Language
        readLang (Lang "en") = Just EN
        readLang (Lang "de") = Just DE
        readLang _           = Nothing
        
msf : MSF (StateT GameState M) Ev ()
msf =  fan_ [ ifIs NewGame newGame
            , ifIs Check check
            , adjLang
            , get >>> dispGame
            ]

export
ui : M (MSF M Ev (), JSIO ())
ui = do
  innerHtmlAt exampleDiv (content EN)
  ini <- randomGame EN
  pure (feedback ini (fromState msf), pure ())
