module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import List exposing (map, range)
import Main exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "canDrop"
        [ test "can drop piece on empty board if not at bottom" <|
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
