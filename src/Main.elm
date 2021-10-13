module Main exposing (..)

import Browser
import Browser.Events exposing (onKeyDown)
import GameDetails
    exposing
        ( Board
        , CurrentPiece
        , Field(..)
        , FieldColor(..)
        , GameDetails
        , Opacity
        , Row(..)
        , Score
        , boardHeight
        , boardWidth
        , canDrop
        , eraseCompleteRows
        , increaseTick
        , lookUp
        , mkEmptyBoard
        , occupiedPositions
        , pieceDefinitions
        , placePieceOnBoard
        , progressFading
        , spawnPiece
        , tPiece
        )
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Json.Decode as Decode
import List exposing (all, concat, drop, head, length, map)
import Random
import String exposing (fromFloat, fromInt)
import Svg
import Svg.Attributes as SA
import Time



-- MAIN


main : Program () Model Msg
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
        _ =
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
