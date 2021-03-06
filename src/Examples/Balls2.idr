module Examples.Balls2

import Data.Either
import Data.List.TR
import Data.MSF.Switch
import Data.Nat
import Data.Vect
import Data.VectorSpace

import Examples.CSS.Colors
import Examples.CSS.Balls
import Examples.Util

import Generics.Derive
import Rhone.Canvas
import Rhone.JS
import Text.CSS.Color

%language ElabReflection
%default total
-- 2D Vector
V2 : Type
V2 = Vect 2 Double

-- Velocity of a point in 2D space
Velocity : Type
Velocity = V2

-- Acceleration of a point in 2D space
Acceleration : Type
Acceleration = V2

-- constant acceleration vector
acc : Acceleration
acc = [0,-9.81]

-- height and width of the room in m
w : Double
w = 10

-- start height of all balls
h0 : Double
h0 = 9

-- ball radius in m
r : Double
r = 0.1

-- start velocity in m/s
v0 : Double
v0 = 4
record Ball where
  constructor MkBall
  col : Color
  pos : V2
  vel : Velocity
inBounds : Ball -> Bool
inBounds (MkBall _ [x,y] _) = y >= 0 && x >= 0 && x <= w

ballToScene : Ball -> Scene
ballToScene b@(MkBall _ [x,y] _) =
  S1 [Fill $ if inBounds b then b.col else transparent] Id $
    circle x (w - y) r Fill
-- room wall thickness in meters
wallThickness : Double
wallThickness = 0.20

-- walls and floor of the room.
walls : Shape
walls =
  let hwt = wallThickness / 2
   in polyLine [(-hwt, 0), (-hwt, w+hwt), (w+hwt,w+hwt), (w+hwt,0)]
ballsToScene : List Ball -> Scene
ballsToScene bs =
  SM  [] (Transform 50 0 0 50 10 10) $
    [ SM [] Id $ mapTR ballToScene bs
    , S1 [Stroke base80, LineWidth wallThickness] Id walls
    ]
public export
data Ev = Run | NumIn | Next DTime

next : Ev -> Event Bits32
next (Next d) = Ev d
next _        = NoEv

%runElab derive "Ev" [Generic,Meta,Show,Eq]

-- canvas width and height
wcanvas : Bits32
wcanvas = 520

content : Node Ev
content =
  div [ class ballsContent ]
    [ lbl "Number of balls:" lblCount
    , input [ ref txtCount
            , onInput (const NumIn)
            , onEnterDown Run
            , class widget
            , placeholder "Range: [1,1000]"
            ] []
    , button [ref btnRun, onClick Run, classes [widget,btn]] ["Run"]
    , div [ref log] []
    , canvas [ref out, width wcanvas, height wcanvas] []
    ]
-- Collision detection: We verify that the given ball
-- is still in the room. If this is not the case, we simulate
-- a bouncing off the walls by inverting the x-velocity (if the
-- ball hit a wall) or the y-velocity (if the ball hit the ground)
checkBounds : Ball -> Ball
checkBounds b@(MkBall c [px,py] [vx,vy]) =
  if      (py <= r  && vy < 0)      then (MkBall c [px,py] [vx,-vy])
  else if (px <= r  && vx < 0)      then (MkBall c [px,py] [-vx,vy])
  else if (px >= (w - r) && vx > 0) then (MkBall c [px,py] [-vx,vy])
  else b

-- moves a ball after a given time delta
-- by adjusting its position and velocity
nextBall : DTime -> Ball -> Ball
nextBall delta (MkBall c p v) =
  let dt   = cast delta / the Double 1000 -- time in seconds
      v2   = v ^+^ (dt *^ acc)
      p2   = p ^+^ (dt / 2 *^ (v ^+^ v2))
   in checkBounds (MkBall c p2 v2)
||| Generates a list of balls to start the simulation.
export
initialBalls : (n : Nat) -> List Ball
initialBalls n = go n Nil
  where col : Bits8 -> Color
        col 0 = comp100
        col 1 = comp80
        col 2 = comp60
        col 3 = comp40
        col _ = comp20

        ball : Nat -> Ball
        ball k =
          let factor = cast {to = Double} k / (cast n - 1.0)
              phi    = pi * factor
              x0     = 1.0 + factor * 8
           in MkBall (col $ cast k `mod` 5) [x0,9] (v0 *^ [sin phi, cos phi])

        go : (k : Nat) -> List Ball -> List Ball
        go 0     bs = bs
        go (S k) bs = go k $ ball k :: bs
public export
M : Type -> Type
M = DomIO Ev JSIO

renderBalls : List Ball -> M ()
renderBalls bs =
  liftJSIO . render $ MkCanvas out (cast wcanvas) (cast wcanvas) (ballsToScene bs)

animation : List Ball -> MSF M Ev ()
animation bs = arr next ?>-
                 [ accumulateWith (mapTR . nextBall) bs >>! renderBalls
                 , fps 15 ?>> showFPS ^>> text log
                 ]
read : String -> Either String (List Ball)
read s =
  let n = cast {to = Nat} s
   in if 0 < n && n <= 1000
        then Right (initialBalls n)
        else Left "Enter a number between 1 and 1000"


msf : MSF M Ev ()
msf = drswitchWhen neutral initialBalls animation
  where readInit : MSF M Ev (Either String (List Ball))
        readInit =    getInput NumIn read txtCount
                 >>>  observeWith (isLeft ^>> disabledAt btnRun)

        initialBalls : MSF M Ev (MSFEvent $ List Ball)
        initialBalls =   fan [readInit, is Run]
                     >>> rightOnEvent

export
ui : M (MSF M Ev (), JSIO ())
ui = do
  innerHtmlAt exampleDiv content
  h     <- DomEnv.handler <$> DomIO.env
  clear <- animate (h . Next)
  pure (msf, liftIO clear)
