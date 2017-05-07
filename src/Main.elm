port module Main exposing (main)

import Css exposing (..)
import Date exposing (Date)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as Html
import Html.Events as Html


styles =
    asPairs >> Html.style


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


decodeMail : Decoder Mail
decodeMail =
    decode Mail
        |> requiredAt [ "envelope", "from" ]
            (map (List.head >> Maybe.withDefault "")
                (list (at [ "address" ] string))
            )
        |> requiredAt [ "envelope", "sender" ]
            (map (List.head >> Maybe.withDefault "")
                (list (at [ "address" ] string))
            )
        |> hardcoded []
        |> hardcoded ""
        |> requiredAt [ "body[]" ] string


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
    { mails = Dict.empty
    , nextId = firstId
    }



{- update -}


type Msg
    = Add (Result String Mail)
    | SetView State Int


init : ( Model, Cmd Msg )
init =
    defaultModel ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Add result ->
            case result of
                Ok mail ->
                    { model
                        | mails = Dict.insert model.nextId ( mail, Short ) model.mails
                        , nextId = 1 + model.nextId
                    }
                        ! []

                Err error ->
                    Debug.crash error

        SetView newState id ->
            { model
                | mails =
                    Dict.update id (Maybe.map (\( mail, _ ) -> ( mail, newState ))) model.mails
            }
                ! []


port addMail : (Value -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ addMail (Add << decodeValue decodeMail) ]



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
    Html.div
        [ styles
            [ displayFlex
            , flexFlow1 row
            ]
        ]
        [ Html.div
            [ styles
                [ fontWeight bold
                , width (Css.rem 6)
                ]
            ]
            [ Html.text (tag ++ ":") ]
        , Html.div [] [ Html.text content ]
        ]
