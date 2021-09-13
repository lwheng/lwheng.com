module Main exposing (..)

import Browser
import Browser.Navigation as BN
import Html
import Html.Attributes as Attr
import Html.Events as E
import Http
import Json.Encode as Encode
import Url exposing (Url)


main =
    Browser.application { init = init, view = view, update = update, subscriptions = subscriptions, onUrlRequest = onUrlRequest, onUrlChange = onUrlChange }


type alias Model =
    { name : String
    , greeting : String
    }


type Msg
    = DoNothing
    | CallAPI
    | CaptureName String
    | GotGreeting (Result Http.Error String)


initModel : Model
initModel =
    { name = "", greeting = "" }


init : Int -> Url -> BN.Key -> ( Model, Cmd Msg )
init _ _ _ =
    ( initModel, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = "lwheng.com"
    , body =
        [ Html.text "Hello, world! This is an Elm application"
        , Html.textarea [ Attr.placeholder "Enter your name", E.onInput CaptureName ] []
        , Html.button [ E.onClick CallAPI ] [ Html.text "Call API" ]
        , Html.label [] [ Html.text "Greeting = " ]
        , Html.label [] [ Html.text model.greeting ]
        ]
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DoNothing ->
            ( model, Cmd.none )

        CallAPI ->
            ( model, callSayHello model )

        CaptureName s ->
            ( { model | name = s }, Cmd.none )

        GotGreeting result ->
            case result of
                Ok ok ->
                    ( { model | greeting = ok }, Cmd.none )

                Err e ->
                    case e of
                        Http.BadUrl s ->
                            ( { model | greeting = "Error: BadUrl " ++ s }, Cmd.none )

                        Http.Timeout ->
                            ( { model | greeting = "Error: Timeout" }, Cmd.none )

                        Http.NetworkError ->
                            ( { model | greeting = "Error: NetworkError" }, Cmd.none )

                        Http.BadStatus code ->
                            ( { model | greeting = "Error: BadStatus " ++ String.fromInt code }, Cmd.none )

                        Http.BadBody s ->
                            ( { model | greeting = "Error: BadBody " ++ s }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


onUrlRequest : Browser.UrlRequest -> Msg
onUrlRequest _ =
    DoNothing


onUrlChange : Url -> Msg
onUrlChange _ =
    DoNothing


callSayHello : Model -> Cmd Msg
callSayHello model =
    Http.post
        { url = "https://lwheng-com-gateway-dl7mzoit.de.gateway.dev/hello"
        , body = Http.jsonBody <| Encode.object [ ( "message", Encode.string model.name ) ]
        , expect = Http.expectString GotGreeting
        }
