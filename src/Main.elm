module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Set exposing (Set)


type alias Model =
    ( Player
    , Board
    )


type alias Board =
    { a : Maybe Player
    , b : Maybe Player
    , c : Maybe Player
    , d : Maybe Player
    , e : Maybe Player
    , f : Maybe Player
    , g : Maybe Player
    }


type alias Msg =
    Model


type Position
    = A
    | B
    | C
    | D
    | E
    | F
    | G


type Player
    = P1
    | P2
    | P3
    | P4
    | P5


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = always
        }


init : Model
init =
    ( P1
    , { a = Just P2
      , b = Just P3
      , c = Just P1
      , d = Nothing
      , e = Just P4
      , f = Nothing
      , g = Just P5
      }
    )


view : Model -> Html Msg
view model =
    innerView Set.empty model


innerView : Set String -> Model -> Html Msg
innerView visited model =
    Html.div
        [ Html.Attributes.style "display" "flex"
        , Html.Attributes.style "flex-direction" "column"
        , Html.Attributes.style "gap" "8px"
        ]
        [ Html.node "style" [] [ Html.text style ]
        , Html.div [] [ viewPosition model ]
        , let
            children : List Model
            children =
                reachable model
          in
          if
            Set.member (toString model) visited
                || (Set.size visited > 1 && List.length children > 1)
          then
            Html.text "---"

          else
            Html.div
                [ Html.Attributes.style "display" "flex"
                , Html.Attributes.style "gap" "8px"
                ]
                (List.map
                    (innerView (Set.insert (toString model) visited))
                    children
                )
        ]


toString : Model -> String
toString ( turn, board ) =
    Debug.toString ( turn, board )


reachable : Model -> List Model
reachable ( turn, board ) =
    let
        position =
            getPosition turn board

        neighbors =
            case position of
                A ->
                    [ B, C, D ]

                B ->
                    [ A, D, E ]

                C ->
                    [ A, D, F ]

                D ->
                    [ A, B, C, E, F, G ]

                E ->
                    [ B, D, G ]

                F ->
                    [ C, D, G ]

                G ->
                    [ D, E, F ]

        emptyNeighbors =
            List.filter (\pos -> isEmpty pos board) neighbors
    in
    List.map (\to -> swap position to ( turn, board )) emptyNeighbors


swap : Position -> Position -> Model -> Model
swap from to ( turn, board ) =
    ( next turn
    , board
        |> set from (get to board)
        |> set to (get from board)
    )


next : Player -> Player
next p =
    case p of
        P1 ->
            P2

        P2 ->
            P3

        P3 ->
            P4

        P4 ->
            P5

        P5 ->
            P1


prev : Player -> Player
prev p =
    case p of
        P2 ->
            P1

        P3 ->
            P2

        P4 ->
            P3

        P5 ->
            P4

        P1 ->
            P5


isEmpty : Position -> Board -> Bool
isEmpty position board =
    get position board == Nothing


get : Position -> Board -> Maybe Player
get position board =
    case position of
        A ->
            board.a

        B ->
            board.b

        C ->
            board.c

        D ->
            board.d

        E ->
            board.e

        F ->
            board.f

        G ->
            board.g


set : Position -> Maybe Player -> Board -> Board
set position player board =
    case position of
        A ->
            { board | a = player }

        B ->
            { board | b = player }

        C ->
            { board | c = player }

        D ->
            { board | d = player }

        E ->
            { board | e = player }

        F ->
            { board | f = player }

        G ->
            { board | g = player }


getPosition : Player -> Board -> Position
getPosition turn board =
    if board.a == Just turn then
        A

    else if board.b == Just turn then
        B

    else if board.c == Just turn then
        C

    else if board.d == Just turn then
        D

    else if board.e == Just turn then
        E

    else if board.f == Just turn then
        F

    else if board.g == Just turn then
        B

    else
        Debug.todo "BAD MODEL"


style : String
style =
    """
    * {
        box-sizing: inherit;
    }

    body {
        box-sizing: border-box;
        padding: 8px;
    }

    table {
        border-collapse: collapse;
        border-spacing: 0;
    }

    td {
        text-align: center;
        width: 19px;
    }

    .cell {
        border: 1px solid black;
        padding: 8px;
        width: 38px;
        height: 38px;
    }
    """


viewPosition : Model -> Html Msg
viewPosition (( turn, { a, b, c, d, e, f, g } ) as model) =
    Html.table
        [ Html.Attributes.style "cursor" "pointer"
        , Html.Events.onClick model
        ]
        [ Html.tr []
            [ Html.td [] []
            , Html.td
                [ Html.Attributes.colspan 2
                , Html.Attributes.class "cell"
                ]
                [ num turn a ]
            , Html.td
                [ Html.Attributes.colspan 2
                , Html.Attributes.class "cell"
                ]
                [ num turn b ]
            , Html.td [] []
            ]
        , Html.tr []
            [ Html.td
                [ Html.Attributes.colspan 2
                , Html.Attributes.class "cell"
                ]
                [ num turn c ]
            , Html.td
                [ Html.Attributes.colspan 2
                , Html.Attributes.class "cell"
                ]
                [ num turn d ]
            , Html.td
                [ Html.Attributes.colspan 2
                , Html.Attributes.class "cell"
                ]
                [ num turn e ]
            ]
        , Html.tr []
            [ Html.td [] []
            , Html.td
                [ Html.Attributes.colspan 2
                , Html.Attributes.class "cell"
                ]
                [ num turn f ]
            , Html.td
                [ Html.Attributes.colspan 2
                , Html.Attributes.class "cell"
                ]
                [ num turn g ]
            , Html.td [] []
            ]
        ]


num : Player -> Maybe Player -> Html msg
num turn i =
    let
        label : String
        label =
            case i of
                Nothing ->
                    " "

                Just P1 ->
                    "L"

                Just P2 ->
                    "A"

                Just P3 ->
                    "M"

                Just P4 ->
                    "O"

                Just P5 ->
                    "B"
    in
    if i == Just turn then
        Html.b
            [ Html.Attributes.style "color" "green" ]
            [ Html.text label ]

    else if i == Just (prev turn) then
        Html.b
            [ Html.Attributes.style "color" "gray" ]
            [ Html.text label ]

    else
        Html.text label
