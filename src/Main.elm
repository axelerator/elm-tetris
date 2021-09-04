module Main exposing (..)

import Browser
import Browser.Events exposing (onKeyDown)
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import List exposing (drop, head, length, map, range)
import Random
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
    , currentPiece : Maybe CurrentPiece
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


pieceDefinitions =
    [ tPiece
    , jPiece
    ]


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


boardWidth =
    11


mkEmptyRow columnCount _ =
    Row <| map (\_ -> Empty) (range 1 columnCount)


mkEmptyBoard rows columns =
    { rows = map (mkEmptyRow columns) <| range 1 rows }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        emptyBoard =
            mkEmptyBoard boardHeight boardWidth
    in
    ( { board = emptyBoard
      , currentPiece = Nothing
      }
    , Random.generate NewCurrentPiece (Random.int 0 <| (length pieceDefinitions - 1))
    )



-- UPDATE


type Msg
    = GravityTick Time.Posix
    | KeyDown Key
    | Noop
    | NewCurrentPiece Int


type Key
    = LeftArrow
    | RightArrow
    | DownArrow


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GravityTick _ ->
            dropCurrentPiece model

        KeyDown key ->
            ( movePiece key model
            , Cmd.none
            )

        NewCurrentPiece pieceIndex ->
            ( spawnPiece pieceIndex model
            , Cmd.none
            )

        Noop ->
            ( model
            , Cmd.none
            )


spawnPiece : Int -> Model -> Model
spawnPiece pieceIndex model =
    let
        pieceDef =
            Maybe.withDefault tPiece <| head <| drop pieceIndex pieceDefinitions

        newPiece =
            { position = ( 5, boardHeight - 2 )
            , tiles = pieceDef.tiles
            , color = pieceDef.color
            }
    in
    { model
        | currentPiece = Just newPiece
    }


movePiece : Key -> Model -> Model
movePiece key model =
    case model.currentPiece of
        Nothing ->
            model

        Just currentPiece ->
            let
                ( x, y ) =
                    currentPiece.position

                tiles =
                    currentPiece.tiles

                newPosition =
                    case key of
                        LeftArrow ->
                            ( x - 1, y )

                        RightArrow ->
                            ( x + 1, y )

                        DownArrow ->
                            ( x, y )

                newTiles =
                    case key of
                        LeftArrow ->
                            tiles

                        RightArrow ->
                            tiles

                        DownArrow ->
                            let
                                rotate ( tx, ty ) =
                                    ( -ty, tx )
                            in
                            map rotate tiles

                movedPiece =
                    { currentPiece
                        | position = newPosition
                        , tiles = newTiles
                    }
            in
            { model | currentPiece = Just movedPiece }


canDrop : CurrentPiece -> Board -> Bool
canDrop { position, tiles } board =
    let
        ( x, y ) =
            position

        translateToCurrentPos ( tx, ty ) =
            ( tx + x, ty + y - 1 )

        translatedTiles =
            map translateToCurrentPos tiles

        lookUp ( tx, ty ) { rows } =
            let
                mbRow =
                    List.head <| List.drop ty rows
            in
            case mbRow of
                Nothing ->
                    Nothing

                Just (Row fields) ->
                    List.head <| List.drop tx fields

        isEmpty pos =
            lookUp pos board == Just Empty

        canPlace (( tx, ty ) as pos) =
            if ty < 0 then
                False

            else
                isEmpty pos
    in
    List.all canPlace translatedTiles


dropCurrentPiece : Model -> ( Model, Cmd Msg )
dropCurrentPiece ({ board } as model) =
    case model.currentPiece of
        Nothing ->
            ( model, Cmd.none )

        Just currentPiece ->
            let
                ( x, y ) =
                    currentPiece.position

                nextRow =
                    y - 1

                droppedPiece =
                    { currentPiece | position = ( x, nextRow ) }
            in
            if canDrop currentPiece board then
                ( { model | currentPiece = Just droppedPiece }
                , Cmd.none
                )

            else
                ( { model
                    | board = placePieceOnBoard currentPiece board
                    , currentPiece = Nothing
                  }
                , Random.generate NewCurrentPiece (Random.int 0 <| (length pieceDefinitions - 1))
                )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every 300 GravityTick
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

        "ArrowDown" ->
            KeyDown DownArrow

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


boardView : Board -> Maybe CurrentPiece -> Html Msg
boardView board mbCurrentPiece =
    let
        boardWithCurrentPiece =
            case mbCurrentPiece of
                Nothing ->
                    board

                Just currentPiece ->
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
