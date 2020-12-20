module Snowcream exposing (..)

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
import Html.Attributes
import Html.Events
import Random
import Random.List as Random
import Geometry.Svg as Svg
import Svg exposing (Svg)
import Svg.Attributes
import Triangle2d exposing (Triangle2d)
import Point2d exposing (Point2d)
import Vector2d exposing (Vector2d)
import Axis2d exposing (Axis2d)
import Direction2d exposing (Direction2d)
import Frame2d exposing (Frame2d)
import Circle2d exposing (Circle2d)
import Quantity exposing (Unitless)
import Pixels exposing (Pixels, pixels)
import Angle
import Color exposing (Color)
import Color.Manipulate as Color
import List.Extra as List
import Json.Decode as Decode


-- MODEL 
type alias Model = 
  { tModels : List (TModel TData) 
  , targetCount : Int 
  }

type alias TData = 
  { color : Color }


type alias Flags = ()

init : Flags -> (Model, Cmd Msg)
init flags = 
  ( { tModels = [ triRoot ]
    , targetCount = 1
    }
  , Random.generate ChildT (mkChild triRoot) 
  )


triRoot = 
  { location = Point2d.origin
  , scalevel = 0
  , qt = T.QT0
  , data = { color = Color.rgb 0.9 0.7 0.8 }
  }


growOrPruneTree newTc model =
  let newTModels = List.take newTc model.tModels in
  ( { model | targetCount = newTc, tModels = newTModels }
  , if List.length newTModels < newTc then 
      Random.generate ChildT (growTree newTModels)
    else
      Cmd.none
  )

-- UPDATE
type Msg 
  = NoOp
  | ChildT (TModel TData)
  | ChangeTargetCount Int
  | Foo String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of 
    ChildT childT -> 
      let newModel = { model | tModels = model.tModels ++ [childT] } in
      growOrPruneTree newModel.targetCount newModel

    ChangeTargetCount tc ->
      growOrPruneTree tc model

    _ -> 
      (model, Cmd.none)

-- VIEW
view : Model -> Html Msg 
view model = 
  Html.div []
    [ viewCanvas model
    , Html.div []
        [ Html.input 
            [ Html.Attributes.type_ "range" 
            , Html.Attributes.min "1"
            , Html.Attributes.max "100"
            , Html.Attributes.step "1"
            , Html.Attributes.value (String.fromInt model.targetCount)
            -- , Html.Events.on "change" 
                -- (Decode.map (ChangeTargetCount << round) Decode.float)
            , Html.Events.onInput
                (String.toInt >> Maybe.map ChangeTargetCount >> Maybe.withDefault NoOp)
            , Html.Attributes.style "width" "50%"
            ]
            []
        , Html.div [] 
            [ Html.text (String.fromInt model.targetCount ++ " tiles") ]
        ]
    ]

viewCanvas : Model -> Html Msg 
viewCanvas model = 
  let

    topLeftFrame =
      Frame2d.atPoint (Point2d.pixels -(frameSize/2) (frameSize/2))
        |> Frame2d.reverseY

    origin =
      Svg.circle2d [Svg.Attributes.fill "black"] 
        (Circle2d.withRadius (pixels 10) Point2d.origin) 

    triSw h = 
      Triangle2d.from h 
        (h |> Point2d.translateBy (Vector2d.pixels unit 0))
        (h |> Point2d.translateBy (Vector2d.pixels 0 unit))

    drawTri mt = 
      Svg.triangle2d (shapeAttrs mt.data.color) (mkTriangle mt)

    twin mt = 
      { mt | triangle = Triangle2d.mirrorAcross Axis2d.y mt.triangle }

    scene =
      Svg.relativeTo topLeftFrame
        (Svg.g [] 
           (List.map drawTri model.tModels ++ [])
        )

  in
    Svg.svg
      [ Svg.Attributes.width (String.fromFloat frameSize)
      , Svg.Attributes.height (String.fromFloat frameSize)
      ]
      [ scene ]


type alias TModel a = 
  { location : Point2d Pixels ()
  , scalevel : Int 
  , qt : T.QuarterTurn
  , data : a
  }


growTree : List (TModel TData) -> Random.Generator (TModel TData)
growTree parents =
  T.chooseWithDefault triRoot parents
    |> Random.andThen mkChild

-- put shape on canvas, adjacent to y axis, and put its twin on the canvas

mkChild : TModel TData -> Random.Generator (TModel TData)
mkChild parent =
  let 
    childLocGenerator = 
      T.chooseWithDefault hull [ hull, foot0, foot1 ]
    makeTheChild childQt childLoc =
      let 
        childHueRotation = (T.qtDegrees childQt - 180) * 0.05
        childColor = 
          parent.data.color 
            |> Color.darken 0.05
            |> Color.rotateHue childHueRotation
      in
      { location = childLoc <| mkTriangle parent 
      , scalevel = parent.scalevel - 1
      , qt = childQt 
      , data = { color = childColor }
      }    
  in
  Random.map2 makeTheChild T.qtGenerator childLocGenerator

mkTriangle : TModel TData -> Triangle2d Pixels ()
mkTriangle tm = 
  let edgeLength = unit * pr2 tm.scalevel in
  Triangle2d.from tm.location 
    (tm.location |> Point2d.translateBy (Vector2d.pixels edgeLength 0))
    (tm.location |> Point2d.translateBy (Vector2d.pixels 0 edgeLength))
    |> Triangle2d.rotateAround tm.location 
      (Angle.degrees (T.qtDegrees tm.qt + skewDegrees (mkSkew tm.scalevel)))

mkSkew sca = 
  case sca |> modBy 2 of 
    0 -> T.Straight
    _ -> T.Tilted


skewDegrees skew =
  case skew of 
    T.Straight -> 0
    T.Tilted -> 45

foot0 t2d = 
  let (_, f0, _ ) = Triangle2d.vertices t2d in 
  f0

hull t2d = 
  let (h, f0, _ ) = Triangle2d.vertices t2d in 
  h

foot1 t2d = 
  let (h, f0, f1 ) = Triangle2d.vertices t2d in 
  f1

frameSize = 512
unit = p2 -2 * frameSize

unitScale n =
  p2 n * unit

p2 : Int -> Float 
p2 n =
  pr2 (n * 2)

pr2 : Int -> Float 
pr2 n =
  sqrt 2 ^ toFloat n

shapeAttrs clr = 
  [ Svg.Attributes.strokeWidth "0"
  , Svg.Attributes.fill (Color.toCssString clr)
  , Svg.Attributes.opacity "0.8"
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
