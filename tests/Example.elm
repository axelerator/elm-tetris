module Example exposing (..)

import Expect
import GameDetails exposing (Field(..), FieldColor(..), Row(..), canDrop, eraseCompleteRows, lookUp, mkEmptyBoard, mkEmptyRow, placePieceOnBoard)
import Main exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "Tetris"
        [ describe "eraseCompleteRows"
            [ test "erases full row on bottom" <|
                \_ ->
                    let
                        _ =
                            mkEmptyBoard 3 3

                        alreadyPlacedPiece =
                            { position = ( 1, 0 )
                            , tiles = [ ( -1, 0 ), ( 0, 0 ), ( 1, 0 ) ]
                            , color = Red
                            }

                        emptyRow =
                            mkEmptyRow 3 0

                        expectedRows =
                            [ FadingRow [ Field Red, Field Red, Field Red ] 1
                            , emptyRow
                            , emptyRow
                            ]

                        boardWithATile =
                            placePieceOnBoard alreadyPlacedPiece (mkEmptyBoard 3 3)

                        gameDetails =
                            { board = boardWithATile
                            , currentPiece = Nothing
                            , score = 0
                            , tick = 0
                            }
                    in
                    Expect.equal (eraseCompleteRows gameDetails).board.rows expectedRows
            ]
        , describe "canDrop"
            [ test "lookUp returns Nothing for lookup left outside of board" <|
                \_ ->
                    let
                        emptyBoard =
                            mkEmptyBoard 3 3
                    in
                    Expect.equal (lookUp ( -1, 1 ) emptyBoard) Nothing
            , test "lookUp returns Nothing for lookup below of board" <|
                \_ ->
                    let
                        emptyBoard =
                            mkEmptyBoard 3 3
                    in
                    Expect.equal (lookUp ( -1, 1 ) emptyBoard) Nothing
            , test "can drop piece on empty board if not at bottom" <|
                \_ ->
                    let
                        currentPiece =
                            { position = ( 1, 1 )
                            , tiles = [ ( 0, 0 ) ]
                            , color = Blue
                            }

                        emptyBoard =
                            mkEmptyBoard 3 3
                    in
                    Expect.equal (canDrop currentPiece emptyBoard) True
            , test "cannot drop piece on board if at bottom" <|
                \_ ->
                    let
                        currentPiece =
                            { position = ( 1, 0 )
                            , tiles = [ ( 0, 0 ) ]
                            , color = Blue
                            }

                        emptyBoard =
                            mkEmptyBoard 3 3
                    in
                    Expect.equal (canDrop currentPiece emptyBoard) False
            , test "cannot drop if tile of piece would be outside of board" <|
                \_ ->
                    let
                        currentPiece =
                            { position = ( 1, 1 )
                            , tiles = [ ( 0, -1 ) ]
                            , color = Blue
                            }

                        emptyBoard =
                            mkEmptyBoard 3 3
                    in
                    Expect.equal (canDrop currentPiece emptyBoard) False
            , test "cannot drop if tile of piece would collide with other tile" <|
                \_ ->
                    let
                        currentPiece =
                            { position = ( 1, 1 )
                            , tiles = [ ( 0, 0 ) ]
                            , color = Blue
                            }

                        alreadyPlacedPiece =
                            { position = ( 1, 0 )
                            , tiles = [ ( 0, 0 ) ]
                            , color = Red
                            }

                        boardWithATile =
                            placePieceOnBoard alreadyPlacedPiece (mkEmptyBoard 3 3)
                    in
                    Expect.equal (canDrop currentPiece boardWithATile) False
            ]
        ]
