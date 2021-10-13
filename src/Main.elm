module Main exposing (..)

import Browser
import Browser.Events exposing (onKeyDown)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import List exposing (all, concat, drop, foldr, head, length, map, range)
import Random
import String exposing (fromFloat, fromInt)
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


type Model
    = RunningGame GameDetails
    | GameOver Board Score


type alias Score =
    Int


type alias GameDetails =
    { board : Board
    , currentPiece : Maybe CurrentPiece
    , score : Score
    , tick : Int
    }


type alias CurrentPiece =
    { position : Position
    , tiles : List Position
    , color : FieldColor
    }


type alias Board =
    { rows : List Row
    , emptyRow : Row
    }


type FieldColor
    = Blue
    | Red
    | Purple
    | Green


type Field
    = Empty
    | Field FieldColor


type alias Opacity =
    Float


type Row
    = Row (List Field)
    | FadingRow (List Field) Opacity


type alias Position =
    ( Int, Int )


type alias PieceDefinition =
    { tiles : List Position
    , color : FieldColor
    }


zPiece : PieceDefinition
zPiece =
    { tiles =
        [ ( 0, 0 )
        , ( -1, 1 )
        , ( 0, 1 )
        , ( 1, 0 )
        ]
    , color = Green
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


lPiece : PieceDefinition
lPiece =
    { tiles =
        [ ( 0, 0 )
        , ( -1, 0 )
        , ( 1, 1 )
        , ( 1, 0 )
        ]
    , color = Purple
    }


pieceDefinitions =
    [ tPiece
    , jPiece
    , lPiece
    , zPiece
    ]


updateRow : FieldColor -> Position -> Int -> Row -> Row
updateRow newColor ( x, y ) rowIndex oldRow =
    let
        fields =
            case oldRow of
                Row fs ->
                    fs

                FadingRow fs _ ->
                    fs

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
    let
        emptyRow =
            mkEmptyRow columns 0
    in
    { rows = map (\_ -> emptyRow) <| range 1 rows
    , emptyRow = emptyRow
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        emptyBoard =
            mkEmptyBoard boardHeight boardWidth
    in
    ( RunningGame
        { board = emptyBoard
        , currentPiece = Nothing
        , score = 0
        , tick = 0
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
    case ( msg, model ) of
        ( GravityTick _, RunningGame gameDetails ) ->
            dropCurrentPiece gameDetails

        ( KeyDown key, RunningGame gameDetails ) ->
            ( RunningGame <| movePiece key gameDetails
            , Cmd.none
            )

        ( NewCurrentPiece pieceIndex, RunningGame gameDetails ) ->
            ( gameOverOrNewPiece pieceIndex gameDetails
            , Cmd.none
            )

        _ ->
            ( model
            , Cmd.none
            )


gameOverOrNewPiece : Int -> GameDetails -> Model
gameOverOrNewPiece pieceIndex gameDetails =
    let
        pieceDef =
            Maybe.withDefault tPiece <| head <| drop pieceIndex pieceDefinitions

        newPiece =
            { position = ( 5, boardHeight - 2 )
            , tiles = pieceDef.tiles
            , color = pieceDef.color
            }
    in
    if canDrop newPiece gameDetails.board then
        RunningGame <| spawnPiece pieceIndex gameDetails

    else
        GameOver gameDetails.board gameDetails.score


spawnPiece : Int -> GameDetails -> GameDetails
spawnPiece pieceIndex gameDetails =
    let
        pieceDef =
            Maybe.withDefault tPiece <| head <| drop pieceIndex pieceDefinitions

        newPiece =
            { position = ( 5, boardHeight - 2 )
            , tiles = pieceDef.tiles
            , color = pieceDef.color
            }
    in
    { gameDetails
        | currentPiece = Just newPiece
    }


flip : (a -> b -> c) -> b -> a -> c
flip f a b =
    f b a


movePiece : Key -> GameDetails -> GameDetails
movePiece key gameDetails =
    case gameDetails.currentPiece of
        Nothing ->
            gameDetails

        Just currentPiece ->
            let
                ( x, y ) =
                    currentPiece.position

                ( newPosition, newTiles ) =
                    case key of
                        LeftArrow ->
                            ( ( x - 1, y )
                            , currentPiece.tiles
                            )

                        RightArrow ->
                            ( ( x + 1, y )
                            , currentPiece.tiles
                            )

                        DownArrow ->
                            ( currentPiece.position
                            , map
                                (\( tx, ty ) -> ( -ty, tx ))
                                currentPiece.tiles
                            )

                movedPiece =
                    { currentPiece
                        | position = newPosition
                        , tiles = newTiles
                    }

                canMove =
                    all ((==) (Just Empty)) <|
                        map (flip lookUp gameDetails.board) <|
                            occupiedPositions movedPiece
            in
            if canMove then
                { gameDetails | currentPiece = Just movedPiece }

            else
                gameDetails


occupiedPositions : CurrentPiece -> List Position
occupiedPositions { position, tiles } =
    let
        ( x, y ) =
            position

        translate ( tx, ty ) =
            ( tx + x, ty + y )
    in
    map translate tiles


lookUp : Position -> Board -> Maybe Field
lookUp ( tx, ty ) { rows } =
    let
        mbRow =
            List.head <| List.drop ty rows
    in
    if tx < 0 || ty < 0 then
        Nothing

    else
        case mbRow of
            Nothing ->
                Nothing

            Just (Row fields) ->
                List.head <| List.drop tx fields

            Just (FadingRow fields _) ->
                List.head <| List.drop tx fields


canDrop : CurrentPiece -> Board -> Bool
canDrop { position, tiles } board =
    let
        ( x, y ) =
            position

        translateToCurrentPos ( tx, ty ) =
            ( tx + x, ty + y - 1 )

        translatedTiles =
            map translateToCurrentPos tiles

        isEmpty pos =
            lookUp pos board == Just Empty

        canPlace (( tx, ty ) as pos) =
            if ty < 0 then
                False

            else
                isEmpty pos
    in
    List.all canPlace translatedTiles


increaseTick : GameDetails -> GameDetails
increaseTick gameDetails =
    { gameDetails
        | tick = gameDetails.tick + 1
    }


dropCurrentPiece : GameDetails -> ( Model, Cmd Msg )
dropCurrentPiece ({ board } as gameDetails) =
    if modBy 10 gameDetails.tick == 0 then
        case gameDetails.currentPiece of
            Nothing ->
                ( RunningGame <| increaseTick gameDetails, Cmd.none )

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
                    ( RunningGame <| increaseTick <| progressFading { gameDetails | currentPiece = Just droppedPiece }
                    , Cmd.none
                    )

                else
                    let
                        gameDetailsWithPlacedPiece =
                            { gameDetails
                                | board = placePieceOnBoard currentPiece board
                            }
                    in
                    ( RunningGame <| increaseTick <| eraseCompleteRows gameDetailsWithPlacedPiece
                    , Random.generate NewCurrentPiece (Random.int 0 <| (length pieceDefinitions - 1))
                    )

    else
        ( RunningGame <| increaseTick <| eraseCompleteRows <| progressFading gameDetails
        , Cmd.none
        )


progressFading : GameDetails -> GameDetails
progressFading gameDetails =
    let
        oldBoard =
            gameDetails.board

        fadeRow row =
            case row of
                Row _ ->
                    row

                FadingRow fields opacity ->
                    FadingRow fields (opacity - 0.1)

        fadedRows =
            map fadeRow oldBoard.rows
    in
    { gameDetails
        | board = { oldBoard | rows = fadedRows }
    }


eraseCompleteRows : GameDetails -> GameDetails
eraseCompleteRows ({ board, currentPiece, score } as gameDetails) =
    let
        isFull row =
            case row of
                Row fields ->
                    all (\f -> f /= Empty) fields

                FadingRow fields _ ->
                    all (\f -> f /= Empty) fields

        folder : Row -> ( List Row, List Row ) -> ( List Row, List Row )
        folder row ( nonEmptyRows, header ) =
            case row of
                Row fields ->
                    if isFull row then
                        ( FadingRow fields 1.0 :: nonEmptyRows
                        , header
                        )

                    else
                        ( row :: nonEmptyRows
                        , header
                        )

                FadingRow fields opacity ->
                    if opacity <= 0.1 then
                        ( nonEmptyRows
                        , mkEmptyRow (length fields) 0 :: header
                        )

                    else
                        ( FadingRow fields (opacity - 0.1) :: nonEmptyRows
                        , header
                        )

        ( allNonEmptyRows, finalHeader ) =
            foldr folder ( [], [] ) board.rows

        newRows =
            allNonEmptyRows ++ finalHeader

        newBoard =
            { board | rows = newRows }
    in
    { gameDetails
        | board = newBoard
        , score = score + length finalHeader
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        GameOver _ _ ->
            Sub.none

        RunningGame _ ->
            Sub.batch
                [ Time.every 20 GravityTick
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
    case model of
        GameOver board score ->
            layout score <|
                div []
                    [ div [ class "gameOver" ] [ text "Game Over" ]
                    , boardView board Nothing
                    ]

        RunningGame { board, currentPiece, score } ->
            layout score <| boardView board currentPiece


layout score content =
    div []
        [ div [] [ text "Developing a Web Tetris in Elm" ]
        , div [] [ text "Continuing TONIGHT at 7PM (EST)" ]
        , div [] [ text <| fromInt score ]
        , content
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

        colorInPostions rowIndex row =
            let
                fields =
                    case row of
                        Row fs ->
                            fs

                        FadingRow fs _ ->
                            fs

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


fieldViewForRow : Int -> Opacity -> Int -> Field -> List (Html Msg)
fieldViewForRow rowIndex opacity columnIndex field =
    fieldView opacity field rowIndex columnIndex


rowView : Int -> Row -> List (Html Msg)
rowView rowNumber row =
    let
        empty =
            Field Red
    in
    case row of
        Row fields ->
            concat <| List.indexedMap (fieldViewForRow rowNumber 1.0) fields

        FadingRow fields opacity ->
            concat <| List.indexedMap (fieldViewForRow rowNumber opacity) fields


ffToColor : Field -> String
ffToColor field =
    case field of
        Empty ->
            "gray"

        Field Blue ->
            "blue"

        Field Red ->
            "red"

        Field Purple ->
            "purple"

        Field Green ->
            "green"


fieldView : Opacity -> Field -> Int -> Int -> List (Html Msg)
fieldView opacity field row column =
    case field of
        Empty ->
            [ emptyFieldView row column ]

        _ ->
            [ emptyFieldView row column
            , Svg.rect
                [ SA.x <| fromInt <| column * 11
                , SA.y <| fromInt <| (boardHeight - row) * 11
                , SA.width "10"
                , SA.height "10"
                , SA.fill <| ffToColor field
                , SA.fillOpacity <| fromFloat opacity
                ]
                []
            ]


emptyFieldView : Int -> Int -> Html Msg
emptyFieldView row column =
    Svg.rect
        [ SA.x <| fromInt <| column * 11
        , SA.y <| fromInt <| (boardHeight - row) * 11
        , SA.width "10"
        , SA.height "10"
        , SA.fill "#CCCCCC"
        ]
        []
