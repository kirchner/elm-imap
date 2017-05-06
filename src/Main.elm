module Main exposing (main)

import Date exposing (Date)
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Events as Html


main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



{- model -}


type alias Model =
    { mails : Dict Int ( Mail, State )
    , nextId : Int
    }


firstId : Int
firstId =
    0


type alias Mail =
    { from : Address
    , to : Address
    , ccs : List Address
    , subject : String
    , body : Body
    }


type State
    = Short
    | Middle
    | Full


mail : Mail
mail =
    { from = "foo@bar"
    , to = "bar@foo"
    , ccs = [ "him", "her" ]
    , subject = "some subject"
    , body = "aldkjf adfka ldfkja dlfkjad lfka dlfkja dlfkja dlf aldkjf adfka ldfkja dlfkjad lfka dlfkja dlfkja dlf aldkjf adfka ldfkja dlfkjad lfka dlfkja dlfkja dlf aldkjf adfka ldfkja dlfkjad lfka dlfkja dlfkja dlf aldkjf adfka ldfkja dlfkjad lfka dlfkja dlfkja dlf aldkjf adfka ldfkja dlfkjad lfka dlfkja dlfkja dlf aldkjf adfka ldfkja dlfkjad lfka dlfkja dlfkja dlf aldkjf\n\nadfka ldfkja dlfkjad lfka dlfkja dlfkja dlf aldkjf adfka ldfkja dlfkjad lfka dlfkja dlfkja dlf aldkjf adfka ldfkja dlfkjad lfka dlfkja dlfkja dlf aldkjf adfka ldfkja dlfkjad lfka dlfkja dlfkja dlf aldkjf adfka ldfkja dlfkjad lfka dlfkja dlfkja dlf aldkjf adfka ldfkja dlfkjad lfka dlfkja dlfkja dlf aldkjf adfka ldfkja dlfkjad lfka dlfkja dlfkja dlf jjjjjjjjjjjjjjaldkjf adfka ldfkja dlfkjad lfka dlfkja dlfkja dlf j"
    }


type alias Address =
    String


type alias Body =
    String


defaultModel : Model
defaultModel =
    { mails =
        Dict.fromList
            [ ( 0, ( mail, Short ) )
            , ( 1, ( mail, Short ) )
            , ( 2, ( mail, Short ) )
            ]
    , nextId = 3
    }



{- update -}


type Msg
    = SetView State Int


init : ( Model, Cmd Msg )
init =
    defaultModel ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetView newState id ->
            { model
                | mails =
                    Dict.update id (Maybe.map (\( mail, _ ) -> ( mail, newState ))) model.mails
            }
                ! []


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



{- view -}


view : Model -> Html Msg
view model =
    Html.div [] (List.map viewMail (Dict.toList model.mails))


viewMail : ( Int, ( Mail, State ) ) -> Html Msg
viewMail ( id, ( mail, state ) ) =
    case state of
        Short ->
            viewShort id mail

        Middle ->
            viewMiddle id mail

        Full ->
            viewFull id mail


viewShort : Int -> Mail -> Html Msg
viewShort id mail =
    Html.div
        [ Html.onClick (SetView Middle id) ]
        [ viewTag "from" mail.from
        , viewTag "to" mail.to
        , viewTag "subject" mail.subject
        ]


viewMiddle : Int -> Mail -> Html Msg
viewMiddle id mail =
    let
        shortHeader =
            Html.div []
                [ viewTag "from" mail.from
                , viewTag "to" mail.to
                , viewTag "subject" mail.subject
                ]

        shortBody =
            mail.body
                |> String.split "\n\n"
                |> List.head
                |> Maybe.withDefault ""
                |> (\s -> Html.p [] [ Html.text s ])
    in
        Html.div
            [ Html.onClick (SetView Full id) ]
            [ shortHeader
            , shortBody
            ]


viewFull : Int -> Mail -> Html Msg
viewFull id mail =
    let
        fullHeader =
            Html.div []
                ([ viewTag "from" mail.from
                 , viewTag "to" mail.to
                 , viewTag "subject" mail.subject
                 ]
                    ++ ccs
                )

        ccs =
            mail.ccs
                |> List.map (viewTag "cc")

        fullBody =
            Html.div []
                (mail.body
                    |> String.split "\n\n"
                    |> List.map (\s -> Html.p [] [ Html.text s ])
                )
    in
        Html.div
            [ Html.onClick (SetView Short id) ]
            [ fullHeader
            , fullBody
            ]


viewTag : String -> String -> Html Msg
viewTag tag content =
    Html.div []
        [ Html.div [] [ Html.text (tag ++ ":") ]
        , Html.div [] [ Html.text content ]
        ]
