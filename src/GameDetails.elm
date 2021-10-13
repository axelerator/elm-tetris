module GameDetails exposing (..)

import List exposing (all, drop, foldr, head, length, map, range)


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


type alias Position =
    ( Int, Int )


type FieldColor
    = Blue
    | Red
    | Purple
    | Green


type alias PieceDefinition =
    { tiles : List Position
    , color : FieldColor
    }


type Field
    = Empty
    | Field FieldColor


type alias Opacity =
    Float


type Row
    = Row (List Field)
    | FadingRow (List Field) Opacity


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

        canPlace (( _, ty ) as pos) =
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
eraseCompleteRows ({ board, score } as gameDetails) =
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
