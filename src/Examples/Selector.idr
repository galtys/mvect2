module Examples.Selector

import Examples.CSS
import Examples.Balls2
import Examples.Fractals
import Examples.MathGame2
import Examples.Performance2
import Examples.Reset2
import Rhone.JS

%default total





public export
MSel : Type -> Type
MSel = DomIO String JSIO
{-
export
content1 : Node String
content1 =
  div [ class contentList ]
      [ div [class pageTitle] ["rhone-js: Examples"]
      , div [class contentHeader]
          [ label [class widgetLabel] ["Choose an Example"]
          , select
              [ classes [widget, selectIn, exampleSelector], onChange id]
              [ option [ value "reset", selected True ] ["Counting Clicks"]
              , option [ value "performance" ] ["Performance"]
              , option [ value "fractals" ] ["Fractals"]
              , option [ value "balls" ] ["Bouncing Balls"]
              , option [ value "math" ] ["Math Game"]
              ]
          ]
      , div [class "grid-x"] [
           div [class "large-2 cell", ref exampleDiv] []
           ,div [class "large-10 cell", ref formContentDiv] []      
           ]
      ]
<button class="close-button" aria-label="Close menu" type="button" data-close>
      <span aria-hidden="true">&times;</span>
    </button>            
-}      
export
content : Node String
content =
 div [] [
    div [] [
       ul [class "menu"] 
         [ li [] [a [href "#",onClick "reset"]["Counting Clicks"]]
         , li [] [a [href "#",onClick "table"]["Table"]]
         , li [] [a [href "#",onClick "sheet"]["Sheet"]]         
         , li [] [a [href "#",onClick "performance"]["Performance"]]
         , li [] [a [href "#",onClick "fractals"]["Fractals"]]
         , li [] [a [href "#",onClick "balls"]["Balls"]]
         , li [] [a [href "#",onClick "math"]["Math Game4"]] ]

    ]
--    ,button [class "close-button",Str "aria-label" "Toggle", Str "type" "button", Str "data-toggle" ""] [span [Str "aria-hidden" "true"]["&times;"]]
    
    , div [class "grid-x"] [
           div [class "large-4 cell",ref exampleDiv] []
  
           ,div [class "large-8 cell", ref formContentDiv] []      
           ]
    {-           
    , div [class "grid-x"] [
           div [class "large-4 cell", ref relatedRouteListDiv ] []
           ,div [class "large-8 cell", ref relatedRoutesDiv ] []      
           ]
    

    , div [class "off-canvas position-left",ref offCanvas,Str "data-off-canvas" ""] [
           div [class "large-4 cell", ref exampleDiv] []
           ,div [class "large-8 cell", ref formContentDiv] []      
           ]
    , div [class "off-canvas-content", Str "data-off-canvas-content" ""] [
           div [class "large-4 cell", ref relatedRouteListDiv ] []
           ,div [class "large-8 cell", ref relatedRoutesDiv ] []      
           ]
      -}     
 ]
cleanup : LiftJSIO io => (clean : JSIO ()) -> io ()
cleanup = liftJSIO

msf : MSF MSel String ()
--msf = feedback (pure ()) $ par [arrM cleanup, arrM select] >>> swap
msf = feedback (pure ()) ( par [arrM cleanup, arrM select] >>> swap )
  where select : String -> MSel (JSIO ())
        --select "reset"       = reactimateInDomIni (const (-8)) Reset2.ui
        select "table"       = reactimateInDomIni Ocas Reset2.ui2        
        select "sheet"       = reactimateInDomIni Ocas Reset2.ui3        
        select "performance" = reactimateInDom Performance2.ui
        select "fractals"    = reactimateInDom Fractals.ui
        select "balls"       = reactimateInDom Balls2.ui
        select "math"        = reactimateInDomIni NewGame MathGame2.ui
        select _             = pure (pure ())

export
ui32 : MSel (MSF MSel String (), JSIO ())
ui32 = do
  rawInnerHtmlAt appStyle allRules
  innerHtmlAt contentDiv content
  pure (msf, pure ())
