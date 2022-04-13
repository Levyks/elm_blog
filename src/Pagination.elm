module Pagination exposing (Pagination, fetchPage, pageParser, paginationDecoder, viewPageBtns)

import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (class, href)
import Http
import Http.Detailed
import HttpHelper exposing (SortDirection, WebDetailedData, apiEndpoint, getQueryString, pageParam, searchParam, sizeParam, sortParam, webDataFromResultDetailed)
import Json.Decode as Decode exposing (Decoder, bool, int, list)
import Json.Decode.Pipeline exposing (required)
import Ports exposing (updateQueryParam)
import Url.Parser.Query as Query
import Util exposing (getLast)


type alias Pagination a =
    { content : List a
    , totalPages : Int
    , totalElements : Int
    , last : Bool
    , size : Int
    , number : Int
    , numberOfElements : Int
    , first : Bool
    , empty : Bool
    }


paginationDecoder : Decoder a -> Decoder (Pagination a)
paginationDecoder decoder =
    Decode.succeed Pagination
        |> required "content" (list decoder)
        |> required "totalPages" int
        |> required "totalElements" int
        |> required "last" bool
        |> required "size" int
        |> required "number" int
        |> required "numberOfElements" int
        |> required "first" bool
        |> required "empty" bool


type PageBtnDescription
    = Ellipsis
    | Numbered Int
    | Previous
    | First
    | Last
    | Next


type PageBtn
    = Active String
    | Disabled String
    | Normal String String


getPageBtnsHead : List PageBtnDescription -> List PageBtnDescription
getPageBtnsHead middle =
    case List.head middle of
        Just (Numbered page) ->
            if page > 2 then
                [ First, Ellipsis ]

            else if page > 1 then
                [ First ]

            else
                []

        _ ->
            []


getPageBtnsTail : List PageBtnDescription -> Int -> List PageBtnDescription
getPageBtnsTail middle total =
    case getLast middle of
        Just (Numbered page) ->
            if page < total - 1 then
                [ Ellipsis, Last ]

            else if page < total then
                [ Last ]

            else
                []

        _ ->
            []


getPageBtnsMiddle : Int -> Int -> Int -> List Int
getPageBtnsMiddle current total maxAround =
    let
        minValue =
            max (min current (total - maxAround + 2) - floor (toFloat maxAround / 2)) 1

        maxValue =
            min (minValue + maxAround) total
    in
    List.range minValue maxValue


getPageBtns : Int -> Int -> List PageBtnDescription
getPageBtns current total =
    let
        maxBtnsAroundCurrent =
            4

        btnsMiddle =
            List.map Numbered (getPageBtnsMiddle current total maxBtnsAroundCurrent)

        btnsHead =
            getPageBtnsHead btnsMiddle

        btnsTail =
            getPageBtnsTail btnsMiddle total
    in
    Previous :: btnsHead ++ btnsMiddle ++ btnsTail ++ [ Next ]


viewPageBtns : Pagination a -> (Int -> String) -> Html msg
viewPageBtns pagination getHref =
    nav []
        [ ul [ class "pagination justify-content-center mt-3" ]
            (List.map
                (viewPageBtnFromDescription pagination getHref)
                (getPageBtns (pagination.number + 1) pagination.totalPages)
            )
        ]


viewPageBtnFromDescription : Pagination a -> (Int -> String) -> PageBtnDescription -> Html msg
viewPageBtnFromDescription pagination getHref pageBtnType =
    let
        current =
            pagination.number + 1
    in
    case pageBtnType of
        Previous ->
            if current > 1 then
                viewPageBtn (Normal "«" (getHref (current - 1)))

            else
                viewPageBtn (Disabled "«")

        Next ->
            if current < pagination.totalPages then
                viewPageBtn (Normal "»" (getHref (current + 1)))

            else
                viewPageBtn (Disabled "»")

        First ->
            viewPageBtn (Normal "1" (getHref 1))

        Last ->
            viewPageBtn (Normal (String.fromInt pagination.totalPages) (getHref pagination.totalPages))

        Numbered page ->
            if page == current then
                viewPageBtn (Active (String.fromInt page))

            else
                viewPageBtn (Normal (String.fromInt page) (getHref page))

        Ellipsis ->
            viewPageBtn (Disabled "...")


viewPageBtn : PageBtn -> Html msg
viewPageBtn btn =
    case btn of
        Active label ->
            li [ class "page-item active" ] [ span [ class "page-link" ] [ text label ] ]

        Disabled label ->
            li [ class "page-item disabled" ] [ span [ class "page-link" ] [ text label ] ]

        Normal label link ->
            li [ class "page-item" ] [ a [ class "page-link", href link ] [ text label ] ]


pageParser : Query.Parser Int
pageParser =
    Query.map (Maybe.withDefault 1) (Query.int "page")


type alias FetchPageParamas a msg =
    { endpoint : String
    , decoder : Decoder a
    , msg : WebDetailedData (Pagination a) -> msg
    , page : Int
    , search : String
    , size : Int
    , sort : ( String, SortDirection )
    }


fetchPage : FetchPageParamas a msg -> Cmd msg
fetchPage params =
    Cmd.batch
        [ Http.get
            { url =
                apiEndpoint
                    (params.endpoint
                        ++ getQueryString
                            [ sortParam (Tuple.second params.sort) (Tuple.first params.sort)
                            , sizeParam params.size
                            , pageParam (params.page - 1)
                            , searchParam params.search
                            ]
                    )
            , expect =
                Http.Detailed.expectJson (webDataFromResultDetailed >> params.msg) (paginationDecoder params.decoder)
            }
        , updateQueryParam ( "q", params.search )
        , updateQueryParam ( "page", String.fromInt params.page )
        ]
