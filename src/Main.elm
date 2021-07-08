module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import String exposing (fromInt)
import Svg
import Svg.Attributes as SA



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { board : Board
    }


type alias Board =
    { rows : List Row
    }


type Row
    = EmptyRow


init : () -> ( Model, Cmd Msg )
init _ =
    let
        boardHeight =
            20

        mkEmptyRow _ =
            EmptyRow

        emptyBoard =
            List.map mkEmptyRow <| List.range 1 boardHeight
    in
    ( { board = { rows = emptyBoard } }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Left
    | Right


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Left ->
            ( model
            , Cmd.none
            )

        Right ->
            ( model
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div [] [ text "LOOK MUM, NO SERVER" ]
        , boardView model.board
        ]


boardView : Board -> Html Msg
boardView board =
    let
        rowViews =
            List.concat (List.indexedMap rowView board.rows)
    in
    Svg.svg
        [ SA.width "120"
        , SA.height "120"
        , SA.viewBox "0 0 120 120"
        ]
        rowViews


rowView : Int -> Row -> List (Html Msg)
rowView rowNumber row =
    let
        boardWidth =
            11

        columns =
            List.range 1 11

        fieldViewForRow =
            fieldView rowNumber
    in
    List.map fieldViewForRow columns


fieldView : Int -> Int -> Html Msg
fieldView row column =
    Svg.rect
        [ SA.x <| fromInt <| column * 11
        , SA.y <| fromInt <| row * 11
        , SA.width "10"
        , SA.height "10"
        , SA.fill "gray"
        ]
        []
