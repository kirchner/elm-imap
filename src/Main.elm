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
    { from : List Address
    , to : List Address
    , ccs : List Address
    , bccs : List Address
    , subject : Maybe String
    , body : Body
    , flags : List String
    }


type alias Address =
    String


type alias Body =
    String


type State
    = Short
    | Middle
    | Full


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
    let
        seen =
            List.member "\\Seen" mail.flags
    in
        Html.div
            [ styles
                [ opacity
                    (if seen then
                        (num 0.7)
                     else
                        (num 1)
                    )
                , padding (px 5)
                ]
            ]
            [ Html.div
                [ styles
                    [ backgroundColor (hex "#ccc")
                    , padding (px 8)
                    ]
                ]
                [ case state of
                    Short ->
                        viewShort id mail

                    Middle ->
                        viewMiddle id mail

                    Full ->
                        viewFull id mail
                ]
            ]


viewShort : Int -> Mail -> Html Msg
viewShort id mail =
    Html.div
        [ Html.onClick (SetView Middle id) ]
        [ shortHeader mail ]


viewMiddle : Int -> Mail -> Html Msg
viewMiddle id mail =
    let
        fullBody =
            Html.div
                [ styles
                    [ backgroundColor (rgba 255 255 255 255)
                    , color (rgba 0 0 0 0.87)
                    , marginLeft (px -8)
                    , marginRight (px -8)
                    , marginTop (px 15)
                    , paddingTop (px 20)
                    , paddingBottom (px 60)
                    , maxHeight (px 240)
                    , overflow hidden
                    ]
                ]
                (mail.body
                    |> String.split "\n\n"
                    |> List.map (\s -> Html.p [] [ Html.text s ])
                )
    in
        Html.div
            [ Html.onClick (SetView Full id) ]
            [ shortHeader mail
            , fullBody
            ]


viewFull : Int -> Mail -> Html Msg
viewFull id mail =
    let
        from =
            List.head mail.from
                |> Maybe.withDefault "(no sender)"

        to =
            List.head mail.to
                |> Maybe.withDefault "(no recipient)"

        subject =
            mail.subject
                |> Maybe.withDefault "(no subject)"

        fullHeader =
            Html.div
                [ styles
                    [ fontSize (px 12)
                    , color (rgba 0 0 0 0.54)
                    , paddingTop (px 5)
                    ]
                ]
                ([ viewTag "from" from
                 , viewTag "to" to
                 , viewTag "subject" subject
                 ]
                    ++ ccs
                )

        ccs =
            mail.ccs
                |> List.map (viewTag "cc")

        fullBody =
            Html.div
                [ styles
                    [ backgroundColor (rgba 255 255 255 255)
                    , color (rgba 0 0 0 0.87)
                    , marginLeft (px -8)
                    , marginRight (px -8)
                    , marginTop (px 15)
                    , paddingTop (px 20)
                    , paddingBottom (px 60)
                    ]
                ]
                (mail.body
                    |> String.split "\n\n"
                    |> List.map (\s -> Html.p [] [ Html.text s ])
                )
    in
        Html.div
            [ Html.onClick (SetView Short id) ]
            [ shortHeader mail
            , fullHeader
            , fullBody
            ]


shortHeader : Mail -> Html Msg
shortHeader mail =
    Html.div []
        [ Html.div
            [ styles
                [ fontSize (px 14)
                , color (rgba 0 0 0 0.52)
                ]
            ]
            [ mail.from
                |> List.head
                |> Maybe.withDefault "(no sender)"
                |> Html.text
            ]
        , Html.div
            [ styles
                [ fontSize (px 16)
                , color (rgba 0 0 0 0.87)
                , fontWeight bold
                ]
            ]
            [ mail.subject
                |> Maybe.withDefault "(no subject)"
                |> Html.text
            ]
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



{- decoder -}


decodeMail : Decoder Mail
decodeMail =
    decode Mail
        |> requiredAt [ "envelope", "from" ] (list (at [ "address" ] string))
        |> requiredAt [ "envelope", "sender" ] (list (at [ "address" ] string))
        |> optionalAt [ "envelope", "cc" ] (list (at [ "address" ] string)) []
        |> optionalAt [ "envelope", "bcc" ] (list (at [ "address" ] string)) []
        |> optionalAt [ "envelope", "subject" ] (map Just string) Nothing
        |> requiredAt [ "body[text]" ] string
        |> requiredAt [ "flags" ] (list string)
