module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes as Attr
import Parser as P exposing (..)
import Regex


type alias Model =
    {}


type Msg
    = NoOp


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( {}, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


sheet : String
sheet =
    """[Am]   Hello darkness, my old [G]friend,
I've come to talk with you [Am]again,
Because a regex soft[F]ly cree[C]ping,
Left its seeds while I [F]was slee[C]ping,
The [F]revision that was planted in git [C]blame
Still remains[C] [Am]
[C]Within the [G]sound of [Am]silence."""


regex : Regex.Regex
regex =
    Regex.fromString "(\\[[ABCDEFG]m?\\]|[^[]*)"
        |> Maybe.withDefault Regex.never


type Chord
    = Chord Note Quality


type Note
    = A
    | B
    | C
    | D
    | E
    | F
    | G


type Quality
    = Major
    | Minor


chordParser : Parser Chord
chordParser =
    let
        noteParser =
            P.oneOf
                [ P.succeed A
                    |. symbol "A"
                , P.succeed F
                    |. symbol "F"
                , P.succeed G
                    |. symbol "G"
                , P.succeed C
                    |. symbol "C"
                ]

        qualityParser =
            P.oneOf
                [ P.succeed Minor
                    |. symbol "m"
                , P.succeed Major
                ]
    in
    P.succeed Chord
        |= noteParser
        |= qualityParser


type alias Line =
    List Token


type Token
    = Lyrics String
    | Parsed Chord


lineParser : Parser Line
lineParser =
    P.succeed identity
        |= P.loop [] lineParserHelper


lineParserHelper acc =
    P.oneOf
        [ P.succeed (\v -> Loop (Parsed v :: acc))
            |. symbol "["
            |= chordParser
            |. symbol "]"
        , P.succeed (\v -> Loop (Lyrics v :: acc))
            |= P.getChompedString
                (P.succeed identity
                    |. P.chompIf (\_ -> True)
                    |. P.chompWhile (\c -> c /= '[')
                )
        , P.succeed (Done (List.reverse acc))
        ]


view : Model -> Html Msg
view model =
    let
        lines =
            String.lines sheet
    in
    Html.div [] <|
        List.map
            (\line ->
                Html.div [ Attr.class "line" ]
                    [ Html.span []
                        [ P.run lineParser line
                            |> viewResult
                        ]
                    ]
            )
            lines


viewResult :
    Result (List P.DeadEnd) (List Token)
    -> Html msg
viewResult result =
    let
        viewToken token =
            case token of
                Parsed chord ->
                    Html.span [ Attr.class "token" ]
                        [ Html.text
                            (Debug.toString chord)
                        ]

                Lyrics string ->
                    Html.span []
                        [ Html.text string ]
    in
    Html.div []
        (case result of
            Ok tokens ->
                List.map viewToken tokens

            Err err ->
                [ Html.text (Debug.toString err) ]
        )


viewRegex : List Regex.Match -> Html msg
viewRegex list =
    Html.div []
        (list
            |> List.map .match
            |> List.map
                (\elem ->
                    Html.span [ Attr.class "token" ]
                        [ Html.text elem ]
                )
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )
