module Main exposing (..)

import Browser
import Browser.Events exposing (onKeyDown)
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import List exposing (map, range)
import String exposing (fromInt)
import Svg
import Svg.Attributes as SA
import Time



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
    , currentPiece : CurrentPiece
    }


type alias CurrentPiece =
    { position : Position
    , tiles : List Position
    , color : FieldColor
    }


type alias Board =
    { rows : List Row
    }


type FieldColor
    = Blue
    | Red


type Field
    = Empty
    | Field FieldColor


type Row
    = Row (List Field)


type alias Position =
    ( Int, Int )


type alias PieceDefinition =
    { tiles : List Position
    , color : FieldColor
    }


tPiece : PieceDefinition
tPiece =
    { tiles =
        [ ( 0, 0 )
        , ( 0, 1 )
        , ( 1, 0 )
        , ( -1, 0 )
        ]
    , color = Red
    }


jPiece : PieceDefinition
jPiece =
    { tiles =
        [ ( 0, 0 )
        , ( -1, 0 )
        , ( -1, 1 )
        , ( 1, 0 )
        ]
    , color = Blue
    }


updateRow : FieldColor -> Position -> Int -> Row -> Row
updateRow newColor ( x, y ) rowIndex ((Row fields) as oldRow) =
    let
        updateField fieldPosition field =
            if fieldPosition == x then
                Field newColor

            else
                field
    in
    if y /= rowIndex then
        oldRow

    else
        Row <| List.indexedMap updateField fields


setField : Position -> FieldColor -> Board -> Board
setField position color board =
    let
        oldRows =
            board.rows

        newRows =
            List.indexedMap (updateRow color position) oldRows

        newBoard =
            { board | rows = newRows }
    in
    newBoard


boardHeight =
    20


init : () -> ( Model, Cmd Msg )
init _ =
    let
        mkEmptyRow _ =
            Row <| map (\_ -> Empty) (range 1 11)

        emptyBoard =
            { rows = map mkEmptyRow <| range 1 boardHeight }

        currentPiece =
            { position = ( 5, boardHeight )
            , tiles = tPiece.tiles
            , color = tPiece.color
            }
    in
    ( { board = emptyBoard
      , currentPiece = currentPiece
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = GravityTick Time.Posix
    | KeyDown Key
    | Noop


type Key
    = LeftArrow
    | RightArrow


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GravityTick _ ->
            ( dropCurrentPiece model
            , Cmd.none
            )

        KeyDown key ->
            ( movePiece key model
            , Cmd.none
            )

        Noop ->
            ( model
            , Cmd.none
            )


movePiece : Key -> Model -> Model
movePiece key ({ currentPiece } as model) =
    let
        ( x, y ) =
            currentPiece.position

        newPosition =
            case key of
                LeftArrow ->
                    ( x - 1, y )

                RightArrow ->
                    ( x + 1, y )

        movedPiece =
            { currentPiece | position = newPosition }
    in
    { model | currentPiece = movedPiece }


dropCurrentPiece : Model -> Model
dropCurrentPiece ({ currentPiece } as model) =
    let
        ( x, y ) =
            currentPiece.position

        nextRow =
            if y == 0 then
                20

            else
                y - 1

        droppedPiece =
            { currentPiece | position = ( x, nextRow ) }
    in
    { model | currentPiece = droppedPiece }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every 1000 GravityTick
        , onKeyDown keyDecoder
        ]


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map toKey (Decode.field "key" Decode.string)


toKey : String -> Msg
toKey string =
    case string of
        "ArrowLeft" ->
            KeyDown LeftArrow

        "ArrowRight" ->
            KeyDown RightArrow

        _ ->
            Noop



-- Decode.succeed KeyDown
-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div [] [ text "Developing a Web Tetris in Elm" ]
        , div [] [ text "Continuing TONIGHT at 7PM (EST)" ]
        , boardView model.board model.currentPiece
        ]


placePieceOnBoard : CurrentPiece -> Board -> Board
placePieceOnBoard currentPiece oldBoard =
    let
        ( x, y ) =
            currentPiece.position

        translateTile ( tx, ty ) =
            ( x + tx, y + ty )

        absoluteTilePositions =
            map translateTile currentPiece.tiles

        minRowTaken =
            Maybe.withDefault 0 <|
                List.minimum <|
                    map Tuple.second absoluteTilePositions

        maxRowTaken =
            Maybe.withDefault 0 <|
                List.maximum <|
                    map Tuple.second absoluteTilePositions

        colorInPostions rowIndex (Row fields) =
            let
                equalPos ( tx, ty ) ( px, py ) =
                    tx == px && ty == py

                containedInTakenPositions p =
                    List.any (equalPos p) absoluteTilePositions

                colorField columnIndex field =
                    if containedInTakenPositions ( columnIndex, rowIndex ) then
                        Field currentPiece.color

                    else
                        field
            in
            Row <| List.indexedMap colorField fields

        updateWithPiece rowIndex row =
            if rowIndex < minRowTaken || rowIndex > maxRowTaken then
                row

            else
                colorInPostions rowIndex row

        newRows =
            List.indexedMap updateWithPiece oldBoard.rows
    in
    { oldBoard | rows = newRows }


boardView : Board -> CurrentPiece -> Html Msg
boardView board currentPiece =
    let
        boardWithCurrentPiece =
            placePieceOnBoard currentPiece board

        rowViews =
            List.concat (List.indexedMap rowView boardWithCurrentPiece.rows)
    in
    Svg.svg
        [ SA.width "150"
        , SA.height "250"
        , SA.viewBox "0 0 150 250"
        ]
        rowViews


fieldViewForRow : Int -> Int -> Field -> Html Msg
fieldViewForRow rowIndex columnIndex field =
    fieldView field rowIndex columnIndex


rowView : Int -> Row -> List (Html Msg)
rowView rowNumber row =
    let
        empty =
            Field Red
    in
    case row of
        Row fields ->
            List.indexedMap (fieldViewForRow rowNumber) fields


ffToColor : Field -> String
ffToColor field =
    case field of
        Empty ->
            "gray"

        Field Blue ->
            "blue"

        Field Red ->
            "red"


fieldView : Field -> Int -> Int -> Html Msg
fieldView field row column =
    Svg.rect
        [ SA.x <| fromInt <| column * 11
        , SA.y <| fromInt <| (boardHeight - row) * 11
        , SA.width "10"
        , SA.height "10"
        , SA.fill <| ffToColor field
        ]
        []
