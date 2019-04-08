module Main exposing (AudioUrl, Exposition, ExpositionMetaData, ImageUrl, Model(..), Msg(..), Position(..), Size(..), TextToolContent, Toc(..), TocEntry(..), Tool, ToolContent(..), VideoUrl, Weave, decodeContent, decodeContentHelp, decodeExposition, decodeMaybeInt, decodeMeta, decodePosition, decodeSize, decodeToc, decodeTocEntry, decodeTool, decodeWeave, getExpositionJSON, getTitle, init, main, makePosition, makeSize, subscriptions, update, view, viewExposition)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, field, int, list, map, string, succeed)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode
import VirtualDom



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
    { weaveTitle : String
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
        { url = "test-exposition.json"
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
            div [ id "exposition-content" ]
                [ viewExpositionMeta exposition.expositionMetaData
                , viewExpositionContent exposition
                ]


viewExpositionContent : Exposition -> Html Msg
viewExpositionContent exposition =
    div [ class "exposition" ]
        (List.map viewWeave exposition.expositionWeaves)


viewWeave : Weave -> Html Msg
viewWeave weave =
    div [ class "weave" ]
        (List.append
            [ h1 [] [ i [] [ text weave.weaveTitle ] ] ]
            (List.map viewTool weave.weaveTools)
        )


maybeIntToString : Maybe Int -> String
maybeIntToString value =
    Maybe.withDefault "null" <| Maybe.map String.fromInt value


positionToString : Position -> String
positionToString posi =
    case posi of
        Position pos ->
            let
                ( x, y ) =
                    ( maybeIntToString pos.x, maybeIntToString pos.y )
            in
            "x = " ++ x ++ ", y = " ++ y


sizeToString : Size -> String
sizeToString sizi =
    case sizi of
        Size size ->
            let
                ( w, h ) =
                    ( maybeIntToString size.width, maybeIntToString size.height )
            in
            "width = " ++ w ++ "hieght = " ++ h


viewTool : Tool -> Html Msg
viewTool tool =
    let
        toolProperties =
            [ tool.toolId
            , positionToString tool.position
            , sizeToString tool.size
            ]

        size =
            tool.size

        makeUlOfProperties properties =
            ul [] (List.map (\property -> li [] [ text property ]) properties)
    in
    div [ class "tool" ]
        [ makeUlOfProperties toolProperties
        , viewToolContent tool.toolContent size
        ]


metaToStringList : ExpositionMetaData -> List (List String)
metaToStringList meta =
    [ [ "title ", meta.metaTitle ]
    , [ "date ", meta.metaDate ]
    , [ "authors ", String.join " " meta.metaAuthors ]
    , [ "keywords ", String.join " " meta.metaKeywords ]
    , [ "exp url ", meta.metaExpMainUrl ]
    ]


makeCell : String -> Html Msg
makeCell txt =
    td [] [ text txt ]


makeRow : List (Html Msg) -> Html Msg
makeRow tds =
    tr [] tds


makeTable : List (Html Msg) -> Html Msg
makeTable trs =
    table [] trs


tableHelper : List (List String) -> Html Msg
tableHelper lst =
    -- first turn strings into cells
    let
        cells =
            List.map (List.map makeCell) lst
    in
    -- then turn list of cell list into rows
    let
        rows =
            List.map makeRow cells
    in
    -- finally make a table out of the rows
    makeTable rows


viewExpositionMeta : ExpositionMetaData -> Html Msg
viewExpositionMeta meta =
    div []
        [ h1 [] [ text "Exposition Metadata" ]
        , tableHelper (metaToStringList meta)
        ]


innerHtml : String -> Html.Attribute msg
innerHtml =
    VirtualDom.property "innerHTML" << Json.Encode.string


viewToolContent : ToolContent -> Size -> Html Msg
viewToolContent toolContent size =
    let
        getWidth =
            case size of
                Size x ->
                    x.width

        getHeight =
            case size of
                Size x ->
                    x.height
    in
    case toolContent of
        TextContent textcontent ->
            div [] [ text textcontent ]

        ImageContent imageurl ->
            img [ attribute "src" imageurl ] []

        VideoContent videourl ->
            let
                w =
                    Maybe.withDefault 400 getWidth

                h =
                    Maybe.withDefault 300 getHeight
            in
            video [ width w, height h, controls True ]
                [ source
                    [ attribute "src" videourl ]
                    []
                , text "Your browser does not support <video>"
                ]

        AudioContent audiourl ->
            audio [ controls True ]
                [ source [ attribute "src" audiourl ] []
                , text "Your browser does not support <audio>"
                ]


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
    field "tag" string
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
