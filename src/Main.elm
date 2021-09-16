module Main exposing (..)

import Browser
import Browser.Navigation as BN
import Element as E
import Element.Background as EB
import Element.Input as EI
import Html
import Http
import Url exposing (Url)
import Url.Builder as UrlBuilder


main =
    Browser.application { init = init, view = view, update = update, subscriptions = subscriptions, onUrlRequest = onUrlRequest, onUrlChange = onUrlChange }


type alias Model =
    { greeting : String
    , host : String
    , apiKey : String
    }


type Msg
    = DoNothing
    | CallAPI
    | HandleHello (Result Http.Error String)


initModel : Model
initModel =
    { greeting = "<No greeting yet>"
    , host = ""
    , apiKey = ""
    }


init : { host : String, apiKey : String } -> Url -> BN.Key -> ( Model, Cmd Msg )
init flags _ _ =
    ( { initModel | host = flags.host, apiKey = flags.apiKey }, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = "lwheng.com"
    , body =
        [ E.layout [] <|
            E.column []
                [ E.text "Hello, world!"
                , E.text "This is a Elm application."
                , EI.button [ EB.color <| E.rgb255 238 238 238 ] { onPress = Just CallAPI, label = E.text "Click" }
                , E.text model.greeting
                ]
        ]
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DoNothing ->
            ( model, Cmd.none )

        CallAPI ->
            ( model, callAPI model )

        HandleHello res ->
            case res of
                Ok v ->
                    ( { model | greeting = v }, Cmd.none )

                Err err ->
                    ( { model | greeting = "Error happened!" }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


onUrlRequest : Browser.UrlRequest -> Msg
onUrlRequest _ =
    DoNothing


onUrlChange : Url -> Msg
onUrlChange _ =
    DoNothing


callAPI : Model -> Cmd Msg
callAPI model =
    let
        url =
            { protocol = Url.Https
            , host = model.host
            , port_ = Nothing
            , path = UrlBuilder.absolute [ "hello" ] [ UrlBuilder.string "key" model.apiKey ]
            , query = Nothing
            , fragment = Nothing
            }
    in
    Http.request
        { method = "GET"
        , headers = []
        , url = Url.toString <| url
        , body = Http.emptyBody
        , expect = Http.expectString HandleHello
        , timeout = Nothing
        , tracker = Nothing
        }
