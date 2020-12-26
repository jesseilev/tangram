module Snowcream exposing (..)


-- TODO
-- nice slider ui
-- download image

import Triangle as T exposing (Triangle)
import Browser
import Browser.Dom as Dom
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
  , bgColor : Color
  }

type alias TData =
  { color : Color }

type alias Flags = ()

init : Flags -> (Model, Cmd Msg)
init flags =
  ({ tModels = [ ]
  , targetCount = 0
  , bgColor = Color.red
  }
  , Random.generate GenBackground colorGenerator
  )

    -- |> Dom.getViewport

triRoot : Color -> Random.Generator (TModel TData)
triRoot bgColor =
  Random.map4
    (\r g b a ->
      { location = Point2d.origin |> Point2d.translateBy (Vector2d.pixels 0 -unit)
      , scalevel = -1
      , qt = T.QT0
      , mirrors = [ Axis2d.through Point2d.origin Direction2d.y ]
      , data = { color = Color.weightedMix bgColor (Color.rgba r g b a) 0.1 }
      }
    )
   (Random.float 0 1) (Random.float 0 1) (Random.float 0 1) (Random.float 0.4 0.6)


growOrPruneTree newTc model =
  let newTModels = List.take newTc model.tModels in
  ( { model | targetCount = newTc, tModels = newTModels }
  , if List.length newTModels < newTc then
      Cmd.batch
        [ Random.generate ChildT (childGenerator newTModels)
        , if List.length newTModels == 0 then
            Random.generate GenBackground colorGenerator
          else
            Cmd.none
        ]
    else
      Cmd.none
  )

-- UPDATE
type Msg
  = NoOp
  | ChildT (TModel TData)
  | ChangeTargetCount Int
  | GenBackground Color

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ChildT childT ->
      let newModel = { model | tModels = model.tModels ++ [childT] } in
      growOrPruneTree newModel.targetCount newModel

    ChangeTargetCount tc ->
      growOrPruneTree tc model

    GenBackground c ->
      ( { model | bgColor = c }
      , Cmd.none
      )

    _ ->
      (model, Cmd.none)

-- VIEW
view : Model -> Html Msg
view model =
  Html.div
    [ Html.Attributes.style "background" "black"
    , Html.Attributes.style "height" "100%"
    , Html.Attributes.style "width" "100%"
    , Html.Attributes.style "display" "flex"
    , Html.Attributes.style "justify-content" "center"
    , Html.Attributes.style "position" "absolute"
    , Html.Attributes.style "align-items" "center"
    ]
    [ Html.div
        []
        [ viewCanvas model
        , Html.div []
            [ Html.input
                [ Html.Attributes.type_ "range"
                , Html.Attributes.min "0"
                , Html.Attributes.max "75"
                , Html.Attributes.step "1"
                , Html.Attributes.value (String.fromInt model.targetCount)
                , Html.Events.onInput
                    (String.toInt >> Maybe.map ChangeTargetCount >> Maybe.withDefault NoOp)
                , Html.Attributes.style "width" "100%"
                , Html.Attributes.style "height" "1px"
                , Html.Attributes.style "-webkit-appearance" "none"
                , Html.Attributes.style "background" "white"
                , Html.Attributes.style "outline" "none"
                ]
                []
            , Html.div []
                [ Html.text (String.fromInt model.targetCount ++ " tiles") ]
            ]
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

    bgRect =
      Svg.rect
        [ Svg.Attributes.fill (Color.toCssString model.bgColor)
        , Svg.Attributes.width "100%"
        , Svg.Attributes.height "100%"
        , Svg.Attributes.x "-50%"
        , Svg.Attributes.y "-50%"
        ]
        []

    triSw h =
      Triangle2d.from h
        (h |> Point2d.translateBy (Vector2d.pixels unit 0))
        (h |> Point2d.translateBy (Vector2d.pixels 0 unit))


    drawTri mt =
      let
        regularTri = Svg.triangle2d (shapeAttrs mt.data.color) (mkTriangle mt)
        addMirror : Axis2d Pixels () -> Svg Msg -> Svg Msg
        addMirror mAxis orig = Svg.g [] [ orig, Svg.mirrorAcross mAxis orig ]
      in
        Svg.g [] <| [ List.foldr addMirror regularTri mt.mirrors ]

    scene =
      Svg.relativeTo topLeftFrame
        (Svg.g [] <| List.concat
          [ [ bgRect ]
          , List.map drawTri model.tModels
          , []
          ]
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
  , mirrors : List (Axis2d Pixels ())
  , data : a
  }


childGenerator : List (TModel TData) -> Random.Generator (TModel TData)
childGenerator parents =
  triRoot Color.black |> Random.andThen  -- TODO black is dumb, refactor this
    (\newTri ->
      Random.choose parents
        |> Random.andThen (\(maybeP, _) ->
          Maybe.map mkChild maybeP |> Maybe.withDefault (Random.constant newTri)
        )
    )

colorGenerator : Random.Generator Color
colorGenerator =
  Random.map3 Color.rgb (Random.float 0 1) (Random.float 0 1) (Random.float 0 1)

mkChild : TModel TData -> Random.Generator (TModel TData)
mkChild parent =
  let
    childLocGenerator =
      T.chooseWithDefault hull [ hull, foot0, foot1 ]
    scaleDiffGenerator =
      Random.weighted (1, 0) [ (1, -3), (1, -2), (2, -1), (2, 1), (1, 2) ]

    qtGenerator =
      T.chooseWithDefault T.QT3
        [ T.QT0, T.QT3 ]

    plusMinusGenerator = T.chooseWithDefault 1 [1, -1]

    dynastyGenerator =
      T.chooseWithDefault False [True, False]

    makeTheChild childQt mkChildLoc scaleDiff plusMinus dynasty =
      let
        childScalevel = parent.scalevel + scaleDiff
        childHueRotation = 12 * -plusMinus * pr2 childScalevel
          -- (T.qtDegrees childQt - 180) * 0.1 * pr2 (parent.scalevel - 1)

        childDarken = 0.09 * plusMinus * pr2 childScalevel
          -- (T.qtDegrees childQt - 180) * (pr2 (parent.scalevel - 1)) * 0.002

        childSat = 0.15 * plusMinus * pr2 childScalevel

        childOpac = 0.15 * plusMinus * pr2 childScalevel + (-0.05 * toFloat childScalevel)
          --0.1 * (pr2 -childScalevel - pr2 0)

        childColor =
          parent.data.color
            |> Color.darken childDarken
            |> Color.saturate childSat
            |> Color.rotateHue childHueRotation
            |> Color.fadeIn childOpac

        childLocation = mkChildLoc <| mkTriangle parent
      in
      { location = childLocation
      , scalevel = childScalevel
      , qt = childQt
      , mirrors =
          parent.mirrors ++
            if dynasty then
              [ Axis2d.through childLocation Direction2d.y ]
            else
              []
      , data = { color = childColor }
      }
  in
  Random.map5 makeTheChild qtGenerator childLocGenerator scaleDiffGenerator plusMinusGenerator dynastyGenerator

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
  -- , Svg.Attributes.opacity "0.5"
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
