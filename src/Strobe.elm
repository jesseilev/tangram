module Strobe exposing (..)


import Triangle as T exposing (Triangle)
import Basics.Extra exposing (fractionalModBy)
import Browser
import Browser.Events as Browser
import Debug
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Random
import Random.List as Random
import Geometry.Svg as Svg
import Svg exposing (Svg)
import Svg.Attributes
import Triangle2d
import Point2d
import Vector2d
import Axis2d
import Frame2d exposing (Frame2d)
import Circle2d exposing (Circle2d)
import Quantity exposing (Unitless)
import Pixels exposing (Pixels, pixels)
import Angle
import Color exposing (Color)
import List.Extra as List
import Time exposing (Posix)
import Ease
import Set exposing (Set)

-- MODEL 
type alias Model = 
  { period : Float 
  , timeElapsed : Float
  , flash : Bool
  , isPlaying : Bool
  }

type alias Flags = ()

init : Flags -> (Model, Cmd Msg)
init flags = 
  ( { period = 100, timeElapsed = 0, flash = False, isPlaying = False }
  , Cmd.none
  )


-- UPDATE
type Msg 
  = NoOp
  | Tick Float
  | AdjustPeriod Float
  | TogglePause
  | SetFlash Bool

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of 
    Tick tDelta ->
      let 
        newTE = fractionalModBy model.period (model.timeElapsed + tDelta) 
        nextMsg = 
          case (model.flash, newTE < model.timeElapsed) of 
            (True, _) -> SetFlash False
            (False, True) -> SetFlash True
            _ -> NoOp
      in
      update nextMsg { model | timeElapsed = newTE }
      
    AdjustPeriod p ->
      ( { model | period = p }
      , Cmd.none 
      )

    TogglePause ->
      ( { model | isPlaying = not model.isPlaying }
      , Cmd.none
      )

    SetFlash f ->
      ( { model | flash = f }, Cmd.none )

    _ -> (model, Cmd.none)


-- VIEW
view model = 
  Html.div 
    [ ]
    [ bodyStyleNode (flashColor model)
    , Html.button 
        [ Events.onClick TogglePause ]
        [ Html.text "Start / Stop"]
    , Html.input 
        [ Attr.type_ "range" 
        , Attr.min "50"
        , Attr.max "200"
        , Attr.step "1"
        , Attr.value (String.fromFloat model.period)
        , Events.onInput
            (String.toFloat >> Maybe.map AdjustPeriod >> Maybe.withDefault NoOp)
        , Attr.style "width" "20%"
        , Attr.style "height" "1px"
        , Attr.style "-webkit-appearance" "none"
        , Attr.style "background" "grey"
        , Attr.style "outline" "none"
        ]
        [] 
    , Html.span [Attr.style "color" "grey"] 
      [ Html.text <| "Flash every " ++ String.fromFloat model.period ++ " ms" ]
    ]

bodyStyleNode clr =
  Html.node "style" [] 
    [ Html.text  <| 
      "body { background: " ++ Color.toCssString clr ++ ";}"
    ]

viewCanvas : Model -> Html Msg 
viewCanvas model = 
  let
    frameSize = 512

    topLeftFrame =
      Frame2d.atPoint (Point2d.pixels -(frameSize/2) (frameSize/2))
        |> Frame2d.reverseY
    origin =
      Svg.circle2d [Svg.Attributes.fill (Color.toCssString (currentColor model))] 
        (Circle2d.withRadius (pixels 10) Point2d.origin) 

    scene =
      Svg.relativeTo topLeftFrame
        (Svg.g [] [ origin ])

  in
    Svg.svg
      [ Svg.Attributes.width (String.fromFloat frameSize)
      , Svg.Attributes.height (String.fromFloat frameSize)
      ]
      [ scene ]

greyscale n = 
  Color.rgb n n n

currentColor model =
  (1.0 - model.timeElapsed / model.period)
    |> Ease.inOutExpo
    |> greyscale

flashColor model =
  if model.flash then Color.white else Color.black


-- MAIN
main : Program () Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = \model -> 
      if model.isPlaying then Browser.onAnimationFrameDelta Tick else Sub.none
    }
