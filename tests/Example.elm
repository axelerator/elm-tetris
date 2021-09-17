module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import List exposing (map, range)
import Main exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "Tetris"
        [ describe "eraseCompleteRows"
            [ test "drops rows above multiple cleared rows" <|
                \_ ->
                    let
                        emptyBoard =
                            mkEmptyBoard 3 3

                        droppedPiece =
                            { position = ( 1, 2 )
                            , tiles = [ ( 0, 0 ) ]
                            , color = Blue
                            }

                        alreadyPlacedPiece =
                            { position = ( 1, 0 )
                            , tiles = [ ( -1, 0 ), ( 0, 0 ), ( 1, 0 ) ]
                            , color = Red
                            }

                        otherAlreadyPlacedPiece =
                            { alreadyPlacedPiece
                                | position = ( 1, 1 )
                            }

                        emptyRow =
                            mkEmptyRow 3 0

                        boardWithRowToClear =
                            placePieceOnBoard alreadyPlacedPiece (mkEmptyBoard 3 3)

                        boardWith2RowsToClear =
                            placePieceOnBoard otherAlreadyPlacedPiece boardWithRowToClear

                        boardWithTileToDrop =
                            placePieceOnBoard droppedPiece boardWith2RowsToClear

                        expectedRows =
                            [ Row [ Empty, Field Blue, Empty ]
                            , emptyRow
                            , emptyRow
                            ]
                    in
                    Expect.equal (eraseCompleteRows boardWithTileToDrop).rows expectedRows
            , test "drops rows above cleared rows" <|
                \_ ->
                    let
                        emptyBoard =
                            mkEmptyBoard 3 3

                        droppedPiece =
                            { position = ( 1, 1 )
                            , tiles = [ ( 0, 0 ) ]
                            , color = Blue
                            }

                        alreadyPlacedPiece =
                            { position = ( 1, 0 )
                            , tiles = [ ( -1, 0 ), ( 0, 0 ), ( 1, 0 ) ]
                            , color = Red
                            }

                        emptyRow =
                            mkEmptyRow 3 0

                        boardWithRowToClear =
                            placePieceOnBoard alreadyPlacedPiece (mkEmptyBoard 3 3)

                        boardWithTileToDrop =
                            placePieceOnBoard droppedPiece boardWithRowToClear

                        expectedRows =
                            [ Row [ Empty, Field Blue, Empty ]
                            , emptyRow
                            , emptyRow
                            ]
                    in
                    Expect.equal (eraseCompleteRows boardWithTileToDrop).rows expectedRows
            , test "erases full row on bottom" <|
                \_ ->
                    let
                        emptyBoard =
                            mkEmptyBoard 3 3

                        alreadyPlacedPiece =
                            { position = ( 1, 0 )
                            , tiles = [ ( -1, 0 ), ( 0, 0 ), ( 1, 0 ) ]
                            , color = Red
                            }

                        emptyRow =
                            mkEmptyRow 3 0

                        expectedRows =
                            [ emptyRow, emptyRow, emptyRow ]

                        boardWithATile =
                            placePieceOnBoard alreadyPlacedPiece (mkEmptyBoard 3 3)
                    in
                    Expect.equal (eraseCompleteRows boardWithATile).rows expectedRows
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
