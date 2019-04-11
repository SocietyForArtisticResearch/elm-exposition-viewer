module Main exposing (AudioUrl, Exposition, ExpositionMetaData, ImageUrl, Model(..), Msg(..), Position(..), Size(..), TextToolContent, Toc(..), TocEntry(..), Tool, ToolContent(..), VideoUrl, Weave, decodeContent, decodeContentHelp, decodeExposition, decodeMaybeInt, decodeMeta, decodePosition, decodeSize, decodeToc, decodeTocEntry, decodeTool, decodeWeave, getExpositionJSON, getTitle, init, main, subscriptions, update, view, viewExposition)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Parser
import Html.Parser.Util
import Http
import Json.Decode exposing (Decoder, field, int, list, map, string, succeed)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode
import List.Extra
import Markdown



-- This takes a parsed RC exposition and turns it into HTML.
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
    = Position ( Maybe Int, Maybe Int ) -- x,y


type Size
    = Size ( Maybe Int, Maybe Int ) -- w, h


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
        { url = "test-exposition2.json"
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
        [ h2 [] [ text "Exposition Parser" ]
        , viewExposition model
        ]


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
positionToString (Position ( x, y )) =
    let
        pr arg =
            maybeIntToString arg
    in
    "x = " ++ pr x ++ ", y = " ++ pr y


sizeToString : Size -> String
sizeToString (Size ( w, h )) =
    let
        pr arg =
            maybeIntToString arg
    in
    "w = " ++ pr w ++ ", y = " ++ pr h


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
    div [ class "tool", title <| String.join " " toolProperties ]
        [ viewToolContent tool.toolContent size
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


viewToolContent : ToolContent -> Size -> Html Msg
viewToolContent toolContent (Size ( toolWidth, toolHeight )) =
    case toolContent of
        TextContent textcontent ->
            div [] <| parseHtml textcontent

        --          div [] <| Markdown.toHtml Nothing textcontent
        ImageContent imageurl ->
            img [ attribute "src" imageurl ] []

        VideoContent videourl ->
            let
                w =
                    Maybe.withDefault 400 toolWidth

                h =
                    Maybe.withDefault 300 toolHeight
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



-- HTML parsing


parseHtml : String -> List (Html Msg)
parseHtml htmlString =
    let
        parsed =
            Html.Parser.run htmlString
    in
    case parsed of
        Ok nodes ->
            Html.Parser.Util.toVirtualDom nodes

        Err error ->
            [ div [] [ text "there is some error" ] ]



-- JSON decoders


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


decodePosition : Decoder Position
decodePosition =
    let
        makePosition x y =
            Position ( x, y )
    in
    Json.Decode.map2 makePosition
        (decodeMaybeInt "x")
        (decodeMaybeInt "y")


decodeSize : Decoder Size
decodeSize =
    let
        makeSize w h =
            Size ( w, h )
    in
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



-- SORTING / STRUCTURING
-- Since elm does not have <$> , we use Maybe.map2


diff : Position -> Position -> Position
diff a b =
    Position ( diffx a b, diffy a b )


magnitude : Position -> Maybe Float
magnitude pos =
    case pos of
        Position ( Just x, Just y ) ->
            let
                xf =
                    toFloat x

                yf =
                    toFloat y
            in
            Just (sqrt ((xf * xf) + (yf * yf)))

        Position _ ->
            Nothing


diffx : Position -> Position -> Maybe Int
diffx a b =
    case ( a, b ) of
        ( Position ( x1, _ ), Position ( x2, _ ) ) ->
            Maybe.map2 (-) x2 x1


diffy : Position -> Position -> Maybe Int
diffy a b =
    case ( a, b ) of
        ( Position ( _, y1 ), Position ( _, y2 ) ) ->
            Maybe.map2 (-) y2 y1



-- for sorting, we assume all tools without x or y are at 0 (?)


getToolX : Tool -> Int
getToolX tool =
    case tool.position of
        Position ( x, _ ) ->
            Maybe.withDefault 0 x


getToolY : Tool -> Int
getToolY tool =
    case tool.position of
        Position ( _, y ) ->
            Maybe.withDefault 0 y


sortByY : List Tool -> List Tool
sortByY tools =
    List.sortBy getToolY tools


sortByX : List Tool -> List Tool
sortByX tools =
    List.sortBy getToolX tools


groupByCollumn : List Tool -> List (List Tool)
groupByCollumn tools =
    let
        limit =
            100

        -- this is the limit in pixels
        sorted =
            sortByX tools

        isSameCollumn : Tool -> Tool -> Bool
        isSameCollumn a b =
            let
                posa =
                    a.position

                posb =
                    b.position
            in
            case diffx posa posb of
                Just x ->
                    x < limit

                Nothing ->
                    True
    in
    groupWhileSimplify <| List.Extra.groupWhile isSameCollumn tools



-- simplyfies the output of List.Extra.groupWhile


groupWhileSimplify : List ( a, List a ) -> List (List a)
groupWhileSimplify list =
    let
        packer : ( a, List a ) -> List a
        packer ( x, xs ) =
            List.append [ x ] xs
    in
    List.map packer list


sortByCollumnAndY : List Tool -> List Tool
sortByCollumnAndY tools =
    let
        groupedByCollumn =
            groupByCollumn tools

        sortedSubGroups =
            List.map (List.sortBy getToolY) groupedByCollumn
    in
    List.concat sortedSubGroups
