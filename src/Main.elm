module Main exposing (..)

import Browser
import Browser.Navigation as BN
import Html
import Url exposing (Url)


main =
    Browser.application { init = init, view = view, update = update, subscriptions = subscriptions, onUrlRequest = onUrlRequest, onUrlChange = onUrlChange }


type alias Model =
    Int


type Msg
    = DoNothing


initModel : Model
initModel =
    0


init : Int -> Url -> BN.Key -> ( Model, Cmd Msg )
init _ _ _ =
    ( initModel, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = "lwheng.com"
    , body =
        [ Html.text "Hello, world! This is an Elm application"
        ]
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update _ model =
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


onUrlRequest : Browser.UrlRequest -> Msg
onUrlRequest _ =
    DoNothing


onUrlChange : Url -> Msg
onUrlChange _ =
    DoNothing
