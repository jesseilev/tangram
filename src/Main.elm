module Main exposing (..)

import Browser
import Debug
import Html exposing (Html)
import Random

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }

type alias Model = {tiles: List Tile}

type alias Tile = {shape:Shape, transform:Transform, skew:Skew}

type Shape = Square | Triangle | Parallelogram

type alias Transform = {scale:Int, turns:Turns, translation:(Int, Int)}

type Msg = Noop | NewTiles (List Tile)

type Turns = QuarterTurn | HalfTurn | ThreeQuarterTurn | NoTurn

type Skew = Straight | Twisted

view : Model -> Html Msg
view model =
  Html.text (Debug.toString model)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NewTiles newTiles ->
      ( { model | tiles = newTiles }
      , Cmd.none)

    _ ->
      (model, Cmd.none)

init : () -> (Model, Cmd Msg)
init _ =
  ( { tiles = [tile1] }
  , Random.generate NewTiles (Random.list 10 tileGenerator)
  )

tile1 =
  { shape = Triangle
  , transform = transformEmpty
  , skew = Straight
  }

transformEmpty =
  { scale = 0
  , turns = NoTurn
  , translation = (0,0)
  }

tileConstructor shape scale trans turns skew =
  { shape = shape
  , transform = { transformEmpty | scale = scale, translation = trans, turns = turns }
  , skew = skew
  }

tileGenerator : Random.Generator Tile
tileGenerator =
  Random.map5 tileConstructor shapeGenerator scaleGenerator transGenerator turnsGenerator skewGenerator

scaleGenerator =
  Random.int 0 6

transGenerator : Random.Generator (Int, Int)
transGenerator =
  Random.pair (Random.int -100 100) (Random.int -100 100)


turnsGenerator : Random.Generator Turns
turnsGenerator =
  Random.int 0 3
    |> Random.map (\i ->
        case i of
          0 -> NoTurn
          1 -> QuarterTurn
          2 -> HalfTurn
          3 -> ThreeQuarterTurn
          _ -> NoTurn
      )

skewGenerator : Random.Generator Skew
skewGenerator =
  Random.int 0 1
    |> Random.map (\i ->
        case i of
          0 -> Straight
          1 -> Twisted
          _ -> Straight
      )

shapeGenerator : Random.Generator Shape
shapeGenerator =
  Random.int 0 2
    |> Random.map (\i ->
        case i of
          0 -> Square
          1 -> Triangle
          2 -> Parallelogram
          _ -> Square
      )
