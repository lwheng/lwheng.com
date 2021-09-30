module Main exposing (..)

import Browser
import Browser.Navigation as BN
import Element as E
import Element.Background as EB
import Element.Border as Border
import Element.Font as Font
import Element.Input as EI
import Element.Region as ER
import Html
import Html.Attributes as HA
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
# H1

## H2

### H3

#### H4

##### H5

###### H6

The `quick` **brown** __fox__ *jumps* ~~over~~ the lazy dog.

> This is a block quote.

```
main :: IO ()
main = putStrLn "Hello, world!"
```

[Google](https://www.google.com.sg)

This is an unordered list:
- [x] First
- [x] Second
- [x] Third


This is an ordered list:
1. First
2. Second
3. Third

"""


view : Model -> Browser.Document Msg
view model =
    { title = "lwheng.com"
    , body =
        [ E.layout [ E.inFront header ] <|
            E.column [ E.width E.fill ]
                [ header
                , E.el [ E.padding 10, E.width E.fill ] <|
                    case markdownView markdown of
                        Ok rendered ->
                            E.column [ E.width E.fill ] rendered

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
        E.text "Markdown In Elm"


markdownView : String -> Result String (List (E.Element Msg))
markdownView md =
    md
        |> Markdown.Parser.parse
        |> Result.mapError (\e -> e |> List.map Markdown.Parser.deadEndToString |> String.join "\n")
        |> Result.andThen (Markdown.Renderer.render renderer)


renderer : Markdown.Renderer.Renderer (E.Element Msg)
renderer =
    { heading = heading
    , paragraph = E.paragraph [ E.paddingEach { top = 0, right = 0, bottom = 20, left = 0 } ]
    , blockQuote =
        \children ->
            E.el [ E.paddingEach { top = 0, right = 0, bottom = 20, left = 0 } ] <|
                E.column
                    [ Border.color (E.rgb255 145 145 145)
                    , Border.widthEach { top = 0, right = 0, bottom = 0, left = 10 }
                    , E.padding 10
                    , EB.color (E.rgb255 245 245 245)
                    ]
                    children
    , html = Markdown.Html.oneOf []
    , text = E.text
    , codeSpan = code
    , strong = E.row [ Font.bold ]
    , emphasis = E.row [ Font.italic ]
    , strikethrough = E.row [ Font.strike ]
    , hardLineBreak = Html.br [] [] |> E.html
    , link =
        \{ title, destination } body ->
            E.newTabLink
                [ E.htmlAttribute (HA.style "display" "inline-flex") ]
                { url = destination
                , label =
                    E.paragraph
                        [ Font.color (E.rgb255 0 0 255)
                        ]
                        body
                }
    , image =
        \image ->
            case image.title of
                Just title ->
                    E.image [ E.width E.fill ] { src = image.src, description = image.alt }

                Nothing ->
                    E.image [ E.width E.fill ] { src = image.src, description = image.alt }
    , unorderedList =
        \items ->
            E.column
                [ E.spacing 15
                , E.paddingEach { top = 0, right = 0, bottom = 20, left = 0 }
                ]
                (items
                    |> List.map
                        (\(Block.ListItem task children) ->
                            E.row [ E.spacing 5 ]
                                [ E.row
                                    [ E.alignTop ]
                                    ((case task of
                                        Block.IncompleteTask ->
                                            EI.defaultCheckbox False

                                        Block.CompletedTask ->
                                            EI.defaultCheckbox True

                                        Block.NoTask ->
                                            E.text "•"
                                     )
                                        :: E.text " "
                                        :: children
                                    )
                                ]
                        )
                )
    , orderedList =
        \startingIndex items ->
            E.column [ E.spacing 15 ]
                (items
                    |> List.indexedMap
                        (\index itemBlocks ->
                            E.row []
                                (E.el
                                    [ E.paddingEach { top = 0, right = 0, bottom = 20, left = 0 }
                                    ]
                                    (E.text (String.fromInt (index + startingIndex) ++ ". "))
                                    :: itemBlocks
                                )
                        )
                )
    , codeBlock = codeBlock
    , thematicBreak = E.none
    , table = E.column []
    , tableHeader = E.column []
    , tableBody = E.column []
    , tableRow = E.row []
    , tableCell = \maybeAlignment children -> E.paragraph [] children
    , tableHeaderCell = \maybeAlignment children -> E.paragraph [] children
    }


heading : { level : Block.HeadingLevel, rawText : String, children : List (E.Element msg) } -> E.Element msg
heading { level, rawText, children } =
    E.paragraph
        [ Font.size <|
            case level of
                Block.H1 ->
                    48

                Block.H2 ->
                    36

                Block.H3 ->
                    30

                Block.H4 ->
                    26

                Block.H5 ->
                    20

                Block.H6 ->
                    18
        , Font.bold
        , Font.family
            [ Font.external
                { name = "Montserrat"
                , url = "https://fonts.googleapis.com/css?family=Montserrat"
                }
            ]
        , E.paddingEach { top = 0, right = 0, bottom = 20, left = 0 }
        , ER.heading (Block.headingLevelToInt level)
        , E.htmlAttribute
            (HA.attribute "name" (rawTextToId rawText))
        , E.htmlAttribute
            (HA.id (rawTextToId rawText))
        ]
        children


rawTextToId rawText =
    rawText
        |> String.split " "
        |> String.join "-"
        |> String.toLower


code : String -> E.Element msg
code snippet =
    E.el
        [ EB.color
            (E.rgba 0 0 0 0.04)
        , Border.rounded 2
        , E.paddingXY 5 3
        , Font.family
            [ Font.external
                { url = "https://fonts.googleapis.com/css?family=Source+Code+Pro"
                , name = "Source Code Pro"
                }
            ]
        ]
        (E.text snippet)


codeBlock : { body : String, language : Maybe String } -> E.Element msg
codeBlock details =
    E.el [ E.paddingEach { top = 0, right = 0, bottom = 20, left = 0 } ] <|
        E.el
            [ EB.color (E.rgba 0 0 0 0.03)
            , E.htmlAttribute (HA.style "white-space" "pre")
            , E.padding 20
            , Font.family
                [ Font.external
                    { url = "https://fonts.googleapis.com/css?family=Source+Code+Pro"
                    , name = "Source Code Pro"
                    }
                ]
            ]
            (E.text details.body)
