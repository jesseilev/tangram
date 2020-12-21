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
  { tModels = [ ]
  , targetCount = 0
  }
    |> update (ChangeTargetCount 1)

triRoot : Random.Generator (TModel TData)
triRoot = 
  Random.map4
    (\r g b a ->
      { location = Point2d.origin |> Point2d.translateBy (Vector2d.pixels 0 -unit)
      , scalevel = 1
      , qt = T.QT0
      , mirrorAxis = Axis2d.through Point2d.origin Direction2d.y
      , data = { color = Color.rgba r g b 1 }
      }
    )
   (Random.float 0 1) (Random.float 0 1) (Random.float 0 1) (Random.float 0.4 0.6)


growOrPruneTree newTc model =
  let newTModels = List.take newTc model.tModels in
  ( { model | targetCount = newTc, tModels = newTModels }
  , if List.length newTModels < newTc then 
      Random.generate ChildT (childGenerator newTModels)
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
  Html.div 
    [ Html.Attributes.style "background" "lightGrey" ]
    [ viewCanvas model
    , Html.div []
        [ Html.input 
            [ Html.Attributes.type_ "range" 
            , Html.Attributes.min "0"
            , Html.Attributes.max "100"
            , Html.Attributes.step "1"
            , Html.Attributes.value (String.fromInt model.targetCount)
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
      Svg.g [] 
        [ Svg.triangle2d (shapeAttrs mt.data.color) (mkTriangle mt)
        , Svg.triangle2d (shapeAttrs mt.data.color) (twin mt.mirrorAxis (mkTriangle mt))]

    twin axis t = 
      Triangle2d.mirrorAcross axis t

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
  , mirrorAxis : Axis2d Pixels ()
  , data : a
  }


childGenerator : List (TModel TData) -> Random.Generator (TModel TData)
childGenerator parents =
  triRoot |> Random.andThen 
    (\newTri -> 
      Random.choose parents
        |> Random.andThen (\(maybeP, _) -> 
          Maybe.map mkChild maybeP |> Maybe.withDefault (Random.constant newTri)
        )
    )

mkChild : TModel TData -> Random.Generator (TModel TData)
mkChild parent =
  let 
    childLocGenerator = 
      T.chooseWithDefault hull [ hull, foot0, foot1 ]
    scaleDiffGenerator =
      T.chooseWithDefault 1 [ -3, -1, 1, 3 ]

    qtGenerator = 
      T.chooseWithDefault T.QT3        
        [ T.QT0, T.QT3 ]

    plusMinusGenerator = T.chooseWithDefault 1 [1, -1]

    dynastyGenerator = 
      T.chooseWithDefault False [True, False, False, False, False, False, False]

    makeTheChild childQt mkChildLoc scaleDiff plusMinus =
      let 
        childScalevel = parent.scalevel + scaleDiff
        childHueRotation = 8 * -plusMinus * pr2 childScalevel
          -- (T.qtDegrees childQt - 180) * 0.1 * pr2 (parent.scalevel - 1)

        childDarken = 0.1 * plusMinus * pr2 childScalevel
          -- (T.qtDegrees childQt - 180) * (pr2 (parent.scalevel - 1)) * 0.002

        childSat = 0.08 * plusMinus * pr2 childScalevel

        childOpac = 0.5 * plusMinus * pr2 childScalevel
          --0.1 * (pr2 -childScalevel - pr2 0)

        childColor = 
          parent.data.color 
            |> Color.darken childDarken
            |> Color.saturate childSat 
            |> Color.rotateHue childHueRotation
            -- |> Color.fadeIn childOpac

        childLocation = mkChildLoc <| mkTriangle parent
      in
      { location = childLocation 
      , scalevel = childScalevel
      , qt = childQt 
      , mirrorAxis = 
          parent.mirrorAxis
          -- if dynasty then 
          --   Axis2d.through childLocation Direction2d.y 
          -- else 
          --   parent.mirrorAxis
      , data = { color = childColor }
      }    
  in
  Random.map4 makeTheChild qtGenerator childLocGenerator scaleDiffGenerator plusMinusGenerator

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

frameSize = 700
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
  , Svg.Attributes.opacity "0.5"
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
