module Pagination exposing (PageBtnsParams, Pagination, buttons, getPageBtnsParams, pageParser, paginationDecoder)

import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (class, classList, href)
import Json.Decode as Decode exposing (Decoder, bool, int, list)
import Json.Decode.Pipeline exposing (required)
import Layout exposing (maybeRender)
import Time exposing (Month(..))
import Url.Parser.Query as Query


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


filterPageBtn : Pagination a -> Int -> Bool
filterPageBtn pagination page =
    page >= 1 && page <= pagination.totalPages


shouldHaveFirstBtn : List Int -> ( Bool, Bool )
shouldHaveFirstBtn btns =
    case List.head btns of
        Nothing ->
            ( False, False )

        Just page ->
            ( page > 1, page > 2 )


shouldHaveLastBtn : Pagination a -> List Int -> ( Bool, Bool )
shouldHaveLastBtn pagination btns =
    case btns |> List.reverse |> List.head of
        Nothing ->
            ( False, False )

        Just page ->
            ( page < pagination.totalPages - 1, page < pagination.totalPages )


type alias PageBtnsParams =
    { hasFirstBtn : Bool
    , hasFirstEllipsis : Bool
    , middle : List Int
    , hasLastBtn : Bool
    , hasLastEllipsis : Bool
    }


getPageBtnsParams : Pagination a -> PageBtnsParams
getPageBtnsParams pagination =
    let
        currentPage =
            pagination.number + 1

        aroundCurrentBtnsCount =
            2

        middle =
            List.filter (filterPageBtn pagination) (List.range (currentPage - aroundCurrentBtnsCount) (currentPage + aroundCurrentBtnsCount))

        ( hasFirstBtn, hasFirstEllipsis ) =
            shouldHaveFirstBtn middle

        ( hasLastBtn, hasLastEllipsis ) =
            shouldHaveLastBtn pagination middle
    in
    if pagination.totalPages == 0 then
        PageBtnsParams False False [ 1 ] False False

    else
        PageBtnsParams hasFirstBtn hasFirstEllipsis middle hasLastEllipsis hasLastBtn


buttons : Pagination a -> Html msg
buttons pag =
    let
        params =
            getPageBtnsParams pag
    in
    nav []
        [ ul [ class "pagination justify-content-center mt-3" ]
            (buildPaginationBtnsHead pag params
                ++ List.map (mapPageBtn pag) params.middle
                ++ buildPaginationBtnsTail pag params
            )
        ]


buildPaginationBtnsHead : Pagination a -> PageBtnsParams -> List (Html msg)
buildPaginationBtnsHead pag params =
    [ viewPageBtn "«" pag.number False pag.first
    , maybeRender params.hasFirstBtn (viewPageBtn "1" (pag.totalPages - 1) (pag.number == 0) False)
    , maybeRender params.hasFirstEllipsis (viewPageBtn "..." 0 False True)
    ]


buildPaginationBtnsTail : Pagination a -> PageBtnsParams -> List (Html msg)
buildPaginationBtnsTail pag params =
    [ maybeRender params.hasLastEllipsis (viewPageBtn "..." 0 False True)
    , maybeRender params.hasLastBtn (viewPageBtn (String.fromInt pag.totalPages) (pag.totalPages - 1) (pag.number == 0) False)
    , viewPageBtn "»" (pag.number + 2) False pag.last
    ]


viewPageBtn : String -> Int -> Bool -> Bool -> Html msg
viewPageBtn label goto active disabled =
    li
        [ class "page-item"
        , classList [ ( "active", active ), ( "disabled", disabled ) ]
        ]
        [ a
            [ class "page-link"
            , href ("?page=" ++ String.fromInt goto)
            ]
            [ text label ]
        ]


mapPageBtn : Pagination a -> Int -> Html msg
mapPageBtn pag page =
    viewPageBtn (String.fromInt page) page ((pag.number + 1) == page) False


pageParser : Query.Parser Int
pageParser =
    Query.map (Maybe.withDefault 1) (Query.int "page")
