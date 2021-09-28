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
import Markdown.Block as Block
import Markdown.Html
import Markdown.Parser
import Markdown.Renderer
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


initModel : Model
initModel =
    { greeting = "<No greeting yet>"
    , host = ""
    , apiKey = ""
    }


init : { host : String, apiKey : String } -> Url -> BN.Key -> ( Model, Cmd Msg )
init flags _ _ =
    ( { initModel | host = flags.host, apiKey = flags.apiKey }, Cmd.none )


markdown : String
markdown =
    """
  # Hello World

  ```
  This is sample code
  ```
  """


view : Model -> Browser.Document Msg
view model =
    { title = "lwheng.com - Learn Myself Some Cloud"
    , body =
        [ E.layout [ E.inFront header ] <|
            E.column [ E.width E.fill ]
                [ header
                , case markdownView markdown of
                    Ok rendered ->
                        E.column [] rendered

                    Err errs ->
                        E.text "Error"
                ]
        ]
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DoNothing ->
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


header : E.Element Msg
header =
    E.el
        [ E.width E.fill
        , E.spacing 20
        , E.padding 20
        , Border.innerShadow { offset = ( 1, 1 ), size = 2, blur = 2, color = E.rgba255 100 100 100 0.5 }
        , EB.color <| E.rgb255 57 144 17
        , Font.color <| E.rgb255 255 255 255
        , Font.center
        , Font.size 30
        , Font.shadow { offset = ( 1, 1 ), blur = 2, color = E.rgb255 100 100 100 }
        ]
    <|
        E.text "lwheng"


markdownView : String -> Result String (List (E.Element Msg))
markdownView md =
    md
        |> Markdown.Parser.parse
        |> Result.mapError (\e -> e |> List.map Markdown.Parser.deadEndToString |> String.join "\n")
        |> Result.andThen (Markdown.Renderer.render renderer)


renderer : Markdown.Renderer.Renderer (E.Element Msg)
renderer =
    { heading = heading
    , paragraph = \_ -> E.text "paragraph"
    , blockQuote = \_ -> E.text "paragraph"
    , html = Markdown.Html.oneOf []
    , text = E.text
    , codeSpan = \_ -> E.text "codeSpan"
    , strong = \_ -> E.text "strong"
    , emphasis = \_ -> E.text "emphasis"
    , strikethrough = \_ -> E.text "strikethrough"
    , hardLineBreak = Html.br [] [] |> E.html
    , link = \_ _ -> E.text "link"
    , image = \_ -> E.text "image"
    , unorderedList = \_ -> E.text "unorderedList"
    , orderedList = \_ _ -> E.text "orderedList"
    , codeBlock = \_ -> E.text "codeBlock"
    , thematicBreak = E.none
    , table = \_ -> E.text "table"
    , tableHeader = \_ -> E.text "tableHeader"
    , tableBody = \_ -> E.text "tableBody"
    , tableRow = \_ -> E.text "tableRow"
    , tableCell = \_ _ -> E.text "tableCell"
    , tableHeaderCell = \_ _ -> E.text "tableHeaderCell"
    }


heading : { level : Block.HeadingLevel, rawText : String, children : List (E.Element msg) } -> E.Element msg
heading { level, rawText, children } =
    E.paragraph
        []
        children
