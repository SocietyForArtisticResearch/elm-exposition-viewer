module Main exposing (AudioUrl, Exposition, ExpositionMetaData, ImageUrl, Model(..), Msg(..), Position(..), Size(..), TextToolContent, Toc(..), TocEntry(..), Tool, ToolContent(..), VideoUrl, Weave, decodeContent, decodeContentHelp, decodeExposition, decodeMaybeInt, decodeMeta, decodePosition, decodeSize, decodeToc, decodeTocEntry, decodeTool, decodeWeave, getExpositionJSON, getTitle, init, main, makePosition, makeSize, subscriptions, update, view, viewExposition)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, field, int, list, map, string, succeed)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type Model
    = Failure String
    | Loading
    | Success Exposition


type Toc
    = Toc (List TocEntry)


type TocEntry
    = TocEntry (List String)


type alias Exposition =
    { expositionToc : Toc
    , expositionId : String
    , expositionMetaData : ExpositionMetaData
    , expositionWeaves : List Weave
    }


type alias Weave =
    { weaveId : String
    , weaveTitle : String
    , weaveUrl : String
    , weaveTools : List Tool
    }


type Position
    = Position
        { x : Maybe Int
        , y : Maybe Int
        }


makePosition : Maybe Int -> Maybe Int -> Position
makePosition a b =
    Position
        { x = a
        , y = b
        }


type Size
    = Size
        { width : Maybe Int
        , height : Maybe Int
        }


makeSize : Maybe Int -> Maybe Int -> Size
makeSize a b =
    Size { width = a, height = b }


type alias TextToolContent =
    String


type alias ImageUrl =
    String


type alias VideoUrl =
    String


type alias AudioUrl =
    String


type alias Tool =
    { toolMediaFile : Maybe String
    , toolId : String
    , position : Position
    , size : Size
    , toolContent : ToolContent
    }



-- makeTool : Maybe String -> String -> Position -> Size -> ToolContent -> Tool
-- makeTool file id position size content =
--     Tool
--         { toolMediaFile = file
--         , toolId = id
--         , position = position
--         , size = size
--         , toolContent = content
--         }


type ToolContent
    = TextContent TextToolContent
    | ImageContent ImageUrl
    | VideoContent VideoUrl
    | AudioContent AudioUrl


type alias ExpositionMetaData =
    { metaTitle : String
    , metaDate : String
    , metaAuthors : List String
    , metaKeywords : List String
    , metaExpMainUrl : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading, getExpositionJSON )


getExpositionJSON : Cmd Msg
getExpositionJSON =
    Http.get
        { url = "http://localhost:8000/src/test-exposition.json"
        , expect = Http.expectJson GotExposition decodeExposition
        }



-- UPDATE


type Msg
    = MorePlease
    | GotExposition (Result Http.Error Exposition)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MorePlease ->
            ( Loading, getExpositionJSON )

        GotExposition result ->
            case result of
                Ok exposition ->
                    ( Success exposition, Cmd.none )

                Err error ->
                    let
                        errorString =
                            httpErrorString error
                    in
                    ( Failure errorString, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


httpErrorString : Http.Error -> String
httpErrorString error =
    case error of
        Http.BadUrl text ->
            "Bad Url: " ++ text

        Http.BadBody text ->
            "Bad body" ++ text

        Http.NetworkError ->
            "Network Error"

        Http.Timeout ->
            "Timeout"

        Http.BadStatus response ->
            "Bad Http Status: " ++ String.fromInt response



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "Random Cats" ]
        , viewExposition model
        ]



-- viewGif : Model -> Html Msg
-- viewGif model =
--     case model of
--         Failure ->
--             div []
--                 [ text "I could not load a random cat for some reason. "
--                 , button [ onClick MorePlease ] [ text "Try Again!" ]
--                 ]
--         Loading ->
--             text "Loading..."
--         Success url ->
--             div []
--                 [ button [ onClick MorePlease, style "display" "block" ] [ text "More Please!" ]
--                 , img [ src url ] []
--                 ]


viewExposition : Model -> Html Msg
viewExposition model =
    case model of
        Failure errorString ->
            div []
                [ text "error"
                , button [ onClick MorePlease ] [ text <| "Try Again!" ++ errorString ]
                ]

        Loading ->
            text "Loading..."

        Success exposition ->
            div [] [ text <| "title =" ++ getTitle exposition ]


getTitle : Exposition -> String
getTitle expo =
    .expositionId expo



-- HTTP
-- getRandomCatGif : Cmd Msg
-- getRandomCatGif =
--     Http.get
--         { url = "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=cat"
--         , expect = Http.expectJson GotGif gifDecoder


decodeExposition : Decoder Exposition
decodeExposition =
    succeed Exposition
        |> required "expositionToc" decodeToc
        |> required "expositionId" string
        |> required "expositionMetaData" decodeMeta
        |> required "expositionWeaves" (list decodeWeave)


decodeToc : Decoder Toc
decodeToc =
    map Toc (list decodeTocEntry)


decodeTocEntry : Decoder TocEntry
decodeTocEntry =
    map TocEntry (list string)


decodeMeta : Decoder ExpositionMetaData
decodeMeta =
    succeed ExpositionMetaData
        |> required "metaTitle" string
        |> required "metaDate" string
        |> required "metaAuthors" (list string)
        |> required "metaKeywords" (list string)
        |> required "metaExpMainUrl" string


decodeWeave : Decoder Weave
decodeWeave =
    let
        decodeTools =
            list decodeTool
    in
    succeed Weave
        |> required "weaveId" string
        |> required "weaveTitle" string
        |> required "weaveUrl" string
        |> required "weaveTools" decodeTools


decodeTool : Decoder Tool
decodeTool =
    succeed Tool
        |> optional "toolMediaFile" (Json.Decode.map Just string) Nothing
        |> required "toolId" string
        |> required "position" decodePosition
        |> required "size" decodeSize
        |> required "toolContent" decodeContent


decodeMaybeInt : String -> Decoder (Maybe Int)
decodeMaybeInt key =
    Json.Decode.maybe (field key int)



--  optional "width" (Json.map Just int) Nothing --


decodePosition : Decoder Position
decodePosition =
    Json.Decode.map2 makePosition
        (decodeMaybeInt "x")
        (decodeMaybeInt "y")


decodeSize : Decoder Size
decodeSize =
    Json.Decode.map2 makeSize
        (decodeMaybeInt "width")
        (decodeMaybeInt "height")


decodeContent : Decoder ToolContent
decodeContent =
    field "Tag" string
        |> Json.Decode.andThen decodeContentHelp


decodeContentHelp : String -> Decoder ToolContent
decodeContentHelp tag =
    case tag of
        "TextContent" ->
            Json.Decode.succeed TextContent |> required "textToolContent" string

        "VideoContent" ->
            Json.Decode.succeed VideoContent |> required "videoUrl" string

        "ImageContent" ->
            Json.Decode.succeed ImageContent |> required "imageUrl" string

        "AudioContent" ->
            Json.Decode.succeed AudioContent |> required "audioUrl" string

        _ ->
            Json.Decode.fail <| "Trying to decode, but tool type"
