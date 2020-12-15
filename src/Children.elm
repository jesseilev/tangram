module Children exposing (..)

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

type alias Triangle a =
  { scalevel : Int
  , quarterturn : QuarterTurn
  , anchor : Point2d
  , skew : Skew
  , data : a
  }

type QuarterTurn = QT0 | QT1 | QT2 | QT3

type Skew = Straight | Tilted

type Scalevel = Scalevel Int

type Vertex = Hull | Foot0 | Foot1

type alias Point2d = Point2d.Point2d Pixels YUpCoordinates
type alias Vector2d = Vector2d.Vector2d Pixels YUpCoordinates
type alias Triangle2d = Triangle2d.Triangle2d Pixels YUpCoordinates

type YUpCoordinates = YUpCoordinates

vertices : Triangle a -> (Point2d, Point2d, Point2d)
vertices t =
  ( hull t, foot0 t, foot1 t )

cannonicalTriangle : Skew -> Triangle2d
cannonicalTriangle skew =
  case skew of
    Straight ->
      Triangle2d.fromVertices
        ( Point2d.origin
        , Point2d.translateBy (Vector2d.pixels 1 -1) Point2d.origin
        , Point2d.translateBy (Vector2d.pixels -1 -1) Point2d.origin
        )

    Tilted ->
      Triangle2d.fromVertices
        ( Point2d.origin
        , Point2d.translateBy (Vector2d.pixels 0 1) Point2d.origin
        , Point2d.translateBy (Vector2d.pixels 1 0) Point2d.origin
        )

mkScale skew scalevel =
  2 ^ (scalevel) |> toFloat

mkTriangle2d : Triangle a -> Triangle2d
mkTriangle2d t =
  let
    scale_ = mkScale t.skew t.scalevel
    displacement =
      Vector2d.from Point2d.origin t.anchor
        |> Vector2d.scaleBy (scale_)
    degs = case t.quarterturn of
      QT0 -> 0
      QT1 -> 90
      QT2 -> 180
      QT3 -> 270
    contri = cannonicalTriangle t.skew
    anchorPoint = v0 contri
  in
    contri
      |> Triangle2d.scaleAbout anchorPoint scale_
      |> Triangle2d.rotateAround anchorPoint (Angle.degrees degs)
      |> Triangle2d.translateBy displacement

v0 : Triangle2d -> Point2d
v0 t2d =
  let (v, _, _) = Triangle2d.vertices t2d in v

v1 : Triangle2d -> Point2d
v1 t2d =
  let (_, v, _) = Triangle2d.vertices t2d in v

v2 : Triangle2d -> Point2d
v2 t2d =
  let (_, _, v) = Triangle2d.vertices t2d in v

hull : Triangle a -> Point2d
hull t =
  v0 (mkTriangle2d t)

foot0 : Triangle a -> Point2d
foot0 t =
  v1 (mkTriangle2d t)

foot1 : Triangle a -> Point2d
foot1 t =
  v2 (mkTriangle2d t)





-----

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model = {triangles: List Tri}

init : () -> (Model, Cmd Msg)
init _ =
  ( { triangles = [ triangle0 ] }
  , Random.generate AddTriangle (triChildGenerator triangle0)
  -- , Cmd.none
  -- Random.generate NewTriangles (Random.list 10 triGenerator)
  )


triangle0 : Tri
triangle0 =
  { scalevel = maxSL
  , quarterturn = QT0
  , skew = Straight
  , anchor = Point2d.pixels 0 0
  , data = { color = Color.red }
  }
--
-- triangle1 : Tri
-- triangle1 =
--   { scalevel = -2
--   , quarterturn = QT0
--   , skew = Tilted
--   , anchor = Point2d.unitless 1 1
--   }


type Msg = Noop | AddTriangle Tri | NewTriangles (List Tri)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =

  case msg of
    -- NewTriangles newTris ->
    --   ( { model | triangles = newTris }
    --   , Cmd.none)

    AddTriangle tri ->
      let
        newTris = tri :: model.triangles
        cmd = if List.length newTris > 1400 then Cmd.none else
          Random.generate AddTriangle
            (chooseWithDefault triangle0 newTris
              |> Random.andThen (\t -> triChildGenerator t)
            )
      in
      ( { model | triangles = newTris }
      , cmd
      )

    _ ->
      (model, Cmd.none)


view : Model -> Html Msg
view model =
  let
    frameSize = 1024
    triangles = Svg.g [] (List.map viewTriangle <| List.reverse model.triangles)
    origin = Svg.circle2d (attrs Color.black) (Circle2d.withRadius (pixels 0.1) Point2d.origin)
    gridMax = 16
    dots =
      List.map
        (\p -> Svg.circle2d (attrs Color.gray) (Circle2d.withRadius (pixels 0.05) p))
        ((List.lift2 (\a b -> (a,b)) (List.range -gridMax gridMax) (List.range -gridMax gridMax))
          |> List.map (\(x, y) -> Point2d.xy (pixels (toFloat x)) (pixels (toFloat y)))
          |> Debug.log "grid"
        )
    topLeftFrame =
      Frame2d.atPoint (Point2d.pixels -(frameSize/2) (frameSize/2))
        |> Frame2d.reverseY
    scene = Svg.relativeTo topLeftFrame
      (Svg.scaleAbout Point2d.origin 32
        (Svg.g [] ([triangles] ++ dots ++ [origin] ))
      )
  in
    Svg.svg
      [ Svg.Attributes.width (String.fromFloat frameSize)
      , Svg.Attributes.height (String.fromFloat frameSize)
      ]
      [scene]

type alias TData =
  { color : Color }

type alias Tri = Triangle TData

viewTriangle : Tri -> Svg Msg
viewTriangle tri =
  let
    t0 = Triangle2d.fromVertices (vertices tri)
    t1 = Triangle2d.mirrorAcross Axis2d.y t0

    drawTri : Triangle2d -> Svg Msg
    drawTri t = Svg.triangle2d (attrs tri.data.color) t

    hl = hullLocation 0 tri
    dot = Svg.circle2d (attrs Color.yellow) (Circle2d.withRadius (pixels 0.2) hl)
  in
  Svg.g [] [ drawTri t0 ]

hullLocation : Int -> Tri -> Point2d.Point2d Pixels YUpCoordinates
hullLocation scalevel tri =
  let
    scaleDiff = scalevel - tri.scalevel
  in
    (hull tri) |> Point2d.scaleAbout Point2d.origin (toFloat(2 ^ tri.scalevel))

foot0Location : Tri -> Point2d.Point2d Pixels YUpCoordinates
foot0Location tri =
  (foot0 tri) |> Point2d.scaleAbout Point2d.origin (toFloat(2 ^ tri.scalevel))


attrs clr =
  [ Svg.Attributes.stroke "blue"
  , Svg.Attributes.strokeWidth "0"
  , Svg.Attributes.strokeLinejoin "round"
  , Svg.Attributes.fill (Color.toCssString clr)
  ]



-- hull s t =
--   v0 t |> Point2d.scaleBy (2 ^ (s - t.scalevel))

mkTri : Int -> QuarterTurn -> Point2d -> Skew -> TData -> Tri
mkTri s qt a sk td =
  { scalevel = s
  , quarterturn = qt
  , anchor = a
  , skew = sk
  , data = td
  }

triChildGenerator : Tri -> Random.Generator Tri
triChildGenerator parent =
  let
    (scaleDiff, sk) = (if parent.skew == Straight then (0, Tilted) else (1, Straight))
    sc = parent.scalevel - scaleDiff
    anchor = foot0Location parent |> Point2d.scaleAbout Point2d.origin scaleDiff
  in
  Random.map2 (\qt td -> mkTri sc qt anchor sk td) qtGenerator tDataGenerator


triGenerator : Random.Generator Tri
triGenerator =
  scalevelGenerator
    |> Random.andThen (\sclv ->
        Random.map5 mkTri (Random.constant sclv) qtGenerator (anchorGenerator sclv) skewGenerator tDataGenerator
      )

tDataGenerator =
  Random.map4 Color.rgba genFloat01 genFloat01 genFloat01 (Random.float 0.1 0.3)
    |> Random.map TData


genFloat01 =
    Random.float 0 1

  -- Random.map4 Triangle scalevelGenerator qtGenerator (anchorGenerator -2) skewGenerator

minSL = -2
maxSL = 3
slDiff = maxSL - minSL
foo = maxSL - 1

scalevelGenerator =
  Random.int minSL maxSL

anchorGenerator : Int -> Random.Generator Point2d
anchorGenerator scalevel =
  let z = 2 ^ -(scalevel - 3) in
  Random.pair (Random.int -z z) (Random.int -z z)
    |> Random.map (\(x,y) -> Point2d.pixels (toFloat x) (toFloat y))

--
qtGenerator : Random.Generator QuarterTurn
qtGenerator =
  chooseWithDefault QT0 [ QT0, QT1, QT2, QT3 ]

skewGenerator : Random.Generator Skew
skewGenerator =
  chooseWithDefault Straight [Straight, Tilted]


chooseWithDefault : a -> List a -> Random.Generator a
chooseWithDefault d xs =
  Random.choose xs
    |> Random.map (\(x, _) -> x |> Maybe.withDefault d)
