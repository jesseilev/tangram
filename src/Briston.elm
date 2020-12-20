module Briston exposing (..)

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
import LineSegment2d
import Frame2d exposing (Frame2d)
import Circle2d exposing (Circle2d)
import Quantity exposing (Unitless)
import Pixels exposing (Pixels, pixels)
import Angle
import Color exposing (Color)
import List.Extra as List

-- import Geometry.Types exposing (LineSegment2d)

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
    frameSize = p2 9

    topLeftFrame =
      Frame2d.atPoint (Point2d.pixels -(frameSize/2) (frameSize/2))
        |> Frame2d.reverseY

    origin =
      Svg.circle2d (shapeAttrs powderBlue2)
        (Circle2d.withRadius (pixels (unitScale -4)) Point2d.origin)

    unit = p2 -2 * frameSize

    unitScale n =
      p2 n * unit

    powderGreen = 
      (Color.rgb 0.5 0.75 0)

    deepBlue =
      Color.rgb 0 0 0.4

    cornflower =
      Color.rgb 0.3 0.4 0.9

    powderBlue =
      Color.rgb 0.55 0.75 0.99

    powderBlue2 =
      Color.rgb 0.6 0.85 0.94

    powderPink = 
      Color.rgb 0.75 0.45 0.6

    lightPowderPink = 
      Color.rgb 0.9 0.7 0.8

    modelTri0 =
      { triangle =
        Triangle2d.from 
          (Point2d.origin)
          (Point2d.pixels unit 0)
          (Point2d.pixels 0 unit)
      , attrs = shapeAttrs powderGreen
      }

    (mt0h, mt0f0, mt0f1) = 
      Triangle2d.vertices modelTri0.triangle

    (mt0l0, mt0b, mt0l1) =
      Triangle2d.edges modelTri0.triangle

    (mt1h, mt1f0, mt1f1) =
      Triangle2d.vertices modelTri1.triangle

    (mt1l0, mt1b, mt1l1) =
      Triangle2d.edges modelTri1.triangle


    modelTri1 =
      { triangle = 
          Triangle2d.from 
            mt0f0
            (mt0f0 |> Point2d.translateBy (Vector2d.pixels unit 0))
            (mt0f0 |> Point2d.translateBy (Vector2d.pixels 0 unit))
            |> Triangle2d.scaleAbout mt0f0 (pr2 1)
            |> Triangle2d.rotateAround mt0f0 (Angle.degrees 45)
      , attrs = shapeAttrs deepBlue
      }

    pinkTri = 
      { triangle = 
          modelTri1.triangle
            |> Triangle2d.scaleAbout mt1h (p2 -3)
      , attrs = shapeAttrs powderPink 
      }

    modelLine0 = 
      { line = 
          LineSegment2d.from
            (LineSegment2d.midpoint mt0b)
            (LineSegment2d.midpoint mt1b)
      , attrs = lineAttrs powderBlue 
      }

    twin mt = 
      { mt | triangle = Triangle2d.mirrorAcross Axis2d.y mt.triangle }

    lineTwin ml = 
      { ml | line = LineSegment2d.mirrorAcross Axis2d.y ml.line }
      
    lineSpine =
      { line = mt0l1
      , attrs = lineAttrs deepBlue
      }

    drawTri t =
      Svg.triangle2d t.attrs t.triangle

    drawLine ml =
      Svg.lineSegment2d ml.attrs ml.line 

    scene =
      Svg.relativeTo topLeftFrame
        (Svg.g [] 
          [ drawTri modelTri0
          , drawTri modelTri1
          , drawTri pinkTri
          , drawTri (twin modelTri0)
          , drawTri (twin modelTri1)
          , drawTri (twin pinkTri)
          , drawLine lineSpine
          , drawLine modelLine0
          , drawLine (lineTwin modelLine0)
          , origin
          ]
        )

  in
    Svg.svg
      [ Svg.Attributes.width (String.fromFloat frameSize)
      , Svg.Attributes.height (String.fromFloat frameSize)
      ]
      [ scene ]

p2 : Int -> Float 
p2 n =
  pr2 (n * 2)

pr2 : Int -> Float 
pr2 n =
  sqrt 2 ^ toFloat n

shapeAttrs clr = 
  [ Svg.Attributes.strokeWidth "0"
  , Svg.Attributes.fill (Color.toCssString clr)
  , Svg.Attributes.opacity "0.9999"
  ]

lineAttrs clr =
  [ Svg.Attributes.strokeWidth "4"
  , Svg.Attributes.stroke (Color.toCssString clr)
  ]


-- MAIN
main : Program () Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }
