module Main exposing (..)

import Browser
import Browser.Navigation as BN
import Element as E
import Element.Background as EB
import Element.Border as Border
import Element.Font as Font
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
        [ E.layout [ E.inFront header ] <|
            E.column [ E.width E.fill ]
                [ header
                , E.row [ E.width E.fill ]
                    [ viewGCP model
                    , viewAWS model
                    ]
                ]
        ]
    }


viewGCP : Model -> E.Element Msg
viewGCP model =
    E.column [ E.width E.fill, E.alignTop ]
        [ headerGCP
        , EI.button [ EB.color <| E.rgb255 238 238 238 ] { onPress = Just CallAPI, label = E.text "Click" }
        , E.text model.greeting
        ]


headerGCP : E.Element Msg
headerGCP =
    E.el
        [ E.width E.fill
        , E.spacing 20
        , E.padding 30
        , Font.center
        , Font.size 30
        , Font.shadow { offset = ( 1, 1 ), blur = 2, color = E.rgb255 100 100 100 }
        ]
    <|
        E.text "LEFT"


headerAWS : E.Element Msg
headerAWS =
    E.el
        [ E.width E.fill
        , E.spacing 20
        , E.padding 30
        , Font.center
        , Font.size 30
        , Font.shadow { offset = ( 1, 1 ), blur = 2, color = E.rgb255 100 100 100 }
        ]
    <|
        E.text "RIGHT"


viewAWS : Model -> E.Element Msg
viewAWS model =
    E.column [ E.width E.fill, E.alignTop ] [ headerAWS ]


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


header : E.Element Msg
header =
    E.el
        [ E.width E.fill
        , E.spacing 20
        , E.padding 30
        , Border.innerShadow { offset = ( 1, 1 ), size = 2, blur = 2, color = E.rgba255 100 100 100 0.5 }
        , EB.color <| E.rgb255 57 144 17
        , Font.color <| E.rgb255 255 255 255
        , Font.center
        , Font.size 30
        , Font.shadow { offset = ( 1, 1 ), blur = 2, color = E.rgb255 100 100 100 }
        ]
    <|
        E.text "Learn Yourself Some Cloud"
