module Vreusche exposing (..)


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



type alias Model = { tris: List Tri }

type alias Tri = Triangle (List (Svg.Attribute Msg))

type alias Flags = ()

init : Flags -> (Model, Cmd Msg)
init flags = 
  ( { tris = 
      [ t0, t1
      ] 
    }
  , Cmd.none
  )

t0 : Tri 
t0 = 
  { scalevel = 2
  , quarterturn = T.QT1
  , skew = T.Straight
  , anchor = Point2d.pixels 1 1
  , data = attrs Color.red
  }

t1 =
  { t0 
    | data = attrs Color.yellow 
    , anchor = T.hull t0 |> Debug.log "t0 hull"
    , scalevel = -1
  }

attrs clr = 
  [ Svg.Attributes.stroke "blue"
  , Svg.Attributes.strokeWidth "0"
  , Svg.Attributes.fill (Color.toCssString clr)
  , Svg.Attributes.opacity "0.5"
  ]

type Msg = NoOp

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of 
    _ -> (model, Cmd.none)


view : Model -> Html Msg 
view model = 
  let 
    frameSize = 1024
    topLeftFrame =
      Frame2d.atPoint (Point2d.pixels -(frameSize/2) (frameSize/2))
        |> Frame2d.reverseY

    gridMax = 16

    gridPts =
      (List.lift2 (\a b -> (a,b)) (List.range -gridMax gridMax) (List.range -gridMax gridMax))
        |> List.map (\(x, y) -> Point2d.xy (pixels (toFloat x)) (pixels (toFloat y)))

    origin =
      Svg.circle2d (attrs Color.black) (Circle2d.withRadius (pixels 0.1) Point2d.origin) 

    dots =
      gridPts 
        |> List.map (\p -> Svg.circle2d (attrs Color.black) (Circle2d.withRadius (pixels 0.05) p))  
        |> (::) origin
        
    
    tris = List.map viewTri model.tris

    scene = Svg.relativeTo topLeftFrame
      (Svg.scaleAbout Point2d.origin 32
        (Svg.g [] (List.concat [tris, dots])
      ))
  in
    Svg.svg
      [ Svg.Attributes.width (String.fromFloat frameSize)
      , Svg.Attributes.height (String.fromFloat frameSize)
      ]
      [ scene ]



viewTri : Tri -> Svg Msg
viewTri tri =
  Svg.triangle2d tri.data (T.triangle2d tri)




main : Program () Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }




type Tree a
  = Tree (FooTri a) (List (ParChi a))

type alias ParChi a =
  { parentVertex : T.Vertex
  , childVertex : T.Vertex
  , child : Tree a
  }

-- a T.Triangle without the Anchor or Skew
type alias FooTri a =
  { scaleShift : Int
  , quarterTurn : T.QuarterTurn  
  , data : a 
  }

treeToStack : Tree a -> List Tri
treeToStack tree =
  [] -- TODO


exampleTree : Tree (List (Svg.Attribute Msg))
exampleTree =
  Tree (FooTri 0 T.QT2 [])
    [ ParChi T.Hull T.Foot0 
        (Tree (FooTri -1 T.QT0 []) [])
    , ParChi T.Foot1 T.Hull
        (Tree (FooTri 0 T.QT3 []) [])
    ]