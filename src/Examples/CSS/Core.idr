module Examples.CSS.Core

import Examples.CSS.Colors
import Rhone.JS
import Text.CSS

--------------------------------------------------------------------------------
--          IDs
--------------------------------------------------------------------------------

||| ID of the `<body>` element. The page content will
||| be placed here.

public export
contentDiv : ElemRef HTMLBodyElement
contentDiv = Id Body "content2"

{-
public export
contentDiv : ElemRef HTMLDivElement
contentDiv = Id Div "content2"
-}
||| The page consists of a static heading with a title an
||| (eventually) a short description of the project.
||| This is followed by a selection box, where visitors can
||| choose an example application.
|||
||| The example application will be dynamicall generated and
||| placed in to a `<div>` with ID `"example"`.
public export
exampleDiv : ElemRef HTMLDivElement
exampleDiv = Id Div "example"

public export
formContentDiv : ElemRef HTMLDivElement
formContentDiv = Id Div "form_content"

public export
relatedRouteListDiv : ElemRef HTMLDivElement
relatedRouteListDiv = Id Div "related_route_list"
public export
relatedRoutesDiv : ElemRef HTMLDivElement
relatedRoutesDiv = Id Div "related_routes"

public export
offCanvas : ElemRef HTMLDivElement
offCanvas = Id Div "off_canvas"

public export
staticTable : ElemRef HTMLTableElement
staticTable = Id Table "spreadsheet"



||| ID of a `<style>` element in the page header.
||| The generated CSS rules will go here.
public export
appStyle : ElemRef HTMLStyleElement
appStyle = Id Style "appstyle"

--------------------------------------------------------------------------------
--          Classes
--------------------------------------------------------------------------------

||| a clickable button
public export
btn : String
btn = "btn"

||| a text input
public export
textIn : String
textIn = "textin"

||| a select input
public export
selectIn : String
selectIn = "selectin"

||| an input widget
public export
widget : String
widget = "widget"

||| a list of input widgets,
||| each on its own line, often with a label
||| on the left.
public export
widgetList : String
widgetList = "widgetList"

||| the main content, split into three rows:
||| a title, the example selector, and the
||| currently loaded example application
public export
contentList : String
contentList = "contentList"

||| the header row where the example selector
||| resides
public export
contentHeader : String
contentHeader = "contentHeader"

||| the row with the page title
public export
pageTitle : String
pageTitle = "pageTitle"

||| the select box used to choose an example
||| application
public export
exampleSelector : String
exampleSelector = "example_selector"

||| a single line in a column
||| of input widgets.
public export
widgetLine : String
widgetLine = "widgetline"

||| a label on the left of an input
||| widget.
public export
widgetLabel : String
widgetLabel = "widgetlabel"

--------------------------------------------------------------------------------
--          Rules
--------------------------------------------------------------------------------

lucida : String
lucida_grande = "Lucida Grande"
fx_event : String
fx_event = "fx_event"

row_hover : Color
row_hover = rgb 221 221 221  --#ddd
font_color : Color
font_color = rgb 64 64 64 --#404040
bg_color : Color
bg_color = rgb 238 238 238 --#EEEEEE

export
coreCSS : List (Rule 1)
coreCSS =
  [ elem Html !!
      [ Height          .= perc 99]
       
  , class fx_event !!
      [ --BorderCollapse .= Collapse
       Width          .= perc 100 ]
  , Pseudo (class fx_event) Hover !!
      [ BackgroundColor .= row_hover ]
  , Many [Class fx_event,Elem Td] !!
      [  Padding         .= All (em 0.2)
      ,  BorderStyle     .= Bottom Solid
      ,  BorderWidth     .= Bottom (px 1)
      ,  BorderColor     .= Bottom row_hover ]
  , Many [Class fx_event,Elem Th] !!
      [  Padding         .= Top (em 0.2)
      ,  Padding         .= Bottom (em 0.2)
      ,  BackgroundColor .= bg_color
      ,  TextAlign       .= Left 
      ,  BorderStyle     .= Bottom Solid
      ,  BorderWidth     .= Bottom (px 1)
      ,  BorderColor     .= Bottom bg_color]
       
  , elem Body !!
      [ BackgroundColor .= bg_color
      , Color           .= base100
      , Display         .= Flex
      , FlexDirection   .= Column
      , FontFamily      .= "\{show lucida_grande}, Helvetica, Verdana, Arial, sans-serif;"
      , Height          .= perc 100
      , Margin          .= px 0
      ]

  , class contentList !!
      [ AlignSelf       .= Center
      , BackgroundColor .= darker_grey
      , Display         .= Flex
      , Flex            .= "1"
      , FlexDirection   .= Column
      , JustifyContent  .= FlexStart
      , Padding         .= VH (px 0) (pt 20)
      , MinWidth        .= perc 80
      ]

  , class pageTitle !!
      [ BorderStyle     .= Bottom Solid
      , BorderWidth     .= Bottom (px 5)
      , BorderColor     .= Bottom base80
      , FontSize        .= XLarge
      , Padding         .= VH (px 40) (px 0)
      , TextAlign       .= Center
      ]

  , class contentHeader !!
      [ Display             .= Grid
      , ColumnGap           .= px 10
      , GridTemplateColumns .= [px 170, fr 1, fr 3]
      , BorderStyle         .= Bottom Solid
      , BorderWidth         .= Bottom (px 2)
      , BorderColor         .= Bottom base80
      , Padding             .= VH (px 30) (px 10)
      ]

  , class widget !!
      [ BackgroundColor .= lighter_grey
      , BorderRadius    .= px 8
      , BorderStyle     .= All Solid
      , BorderWidth     .= px 2
      , BorderColor     .= All comp100
      , Color           .= darker_grey
      , FontSize        .= Large
      , Padding         .= px 3
      ]

  , Pseudo (class widget) Hover !!
      [ BackgroundColor .= lightest_grey
      , BorderColor     .= All comp60
      ]

  , Pseudo (class widget) Active !!
      [ BackgroundColor .= lightest_grey
      , BorderColor     .= All comp60
      ]

  , Pseudo (class widget) FocusVisible !!
      [ BackgroundColor .= lightest_grey
      , BorderColor     .= All comp60
      ]

  , Pseudo (class widget) Disabled !!
      [ BackgroundColor .= light_grey
      , BorderColor     .= All dark_grey
      ]

  , class textIn  !!
      [ TextAlign       .= End ]

  , class selectIn  !!
      [ TextAlign       .= End ]

  , class exampleSelector  !!
      [ FontSize        .= Large
      , GridColumn      .= At 2
      ]

  , Pseudo (class widget) Invalid !!
      [ BorderColor     .= All red ]

  -- deprecated
  , class widgetList !!
      [ ListStyleType   .= None
      , Display         .= Flex
      , FlexDirection   .= Column
      , JustifyContent  .= FlexStart
      ]

  -- deprecated
  , class widgetLine !!
      [ AlignItems      .= FlexStart
      , Display         .= Flex
      , Margin          .= Bottom (px 5)
      ]

  , class widgetLabel !!
      [ FontSize        .= Large ]
  ]
