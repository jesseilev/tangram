module Octavorant exposing (..)

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


-- TYPES

{- 
logarithmic scale

contains info for the scale level and also the skew

goes up by factor of sqrt 2
  
  scaleAmount scaleOctave = 
    let maybeHalf = 
      case scaleOctave.skew of 
        Straight -> 0
        Tilted -> 0.5
    in
      2 ^ (toFloat level + maybeHalf)
-}
type alias Octavorant = 
  { level : Int
  , skew : Skew 
  }

type Skew = Straight | Tilted


fromR2s : R2 -> Octavorant 
fromR2s r2 =
  let


add : Octavorant -> Octavorant -> Octavorant
add o0 o1 =
  fromR2s (r2Add (r2s o0) (r2s o1))


{-
R2

a tagged int

represents scale degree in a logarithmic scale system

`R2 x` represents (sqrt 2) ^ x
-}
type R2 = R2 Int

r2Unwrap : R2 -> Int 
r2Unwrap (R2 x) =x 

r2Run : R2 -> Float 
r2Run (R2 x) = (sqrt 2) ^ (toFloat x)

r2Add : R2 -> R2 -> R2
r2Add (R2 x) (R2 y) = R2 (x + y)

actualScale : Octavorant -> Float 
actualScale oct = r2Run (scaleDegree oct)

r2s: Octavorant -> R2
r2s oct = r2Add (levelR2s oct.level) (skewR2s oct.skew)

levelR2s l = l * 2

skewR2s skew =
  case skew of 
    Straight -> R2 0
    Tilted -> R2 1



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
