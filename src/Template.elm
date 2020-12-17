module Template exposing (..)

-- this is a template meant to be copied
-- so you can get started quickly on a new concept without retyping a bunch of boilerplate
-- 1. duplicate this file 
-- 2. rename the file <MyModuleName>.elm
-- 3. change the word "Template" above: module <MyModuleName> exposing (..)
-- 4. visit in the browser and make sure it works
-- 5. start coding!

import Triangle as T exposing (Triangle)
import Browser
import Debug
import Html exposing (Html)
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

-- MODEL 
type alias Model = 
  {}

type alias Flags = ()

init : Flags -> (Model, Cmd Msg)
init flags = 
  ( {}
  , Cmd.none
  )


-- UPDATE
type Msg = NoOp

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of 
    _ -> (model, Cmd.none)


-- VIEW
view : Model -> Html Msg 
view model = 
  let
    frameSize = 512

    topLeftFrame =
      Frame2d.atPoint (Point2d.pixels -(frameSize/2) (frameSize/2))
        |> Frame2d.reverseY

    origin =
      Svg.circle2d [Svg.Attributes.fill "black"] 
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


-- MAIN
main : Program () Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }
