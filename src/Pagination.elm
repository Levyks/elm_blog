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


buttons : Pagination a -> (Int -> String) -> Html msg
buttons pag getHref =
    let
        params =
            getPageBtnsParams pag
    in
    nav []
        [ ul [ class "pagination justify-content-center mt-3" ]
            (buildPaginationBtnsHead pag getHref params
                ++ List.map (mapPageBtn pag getHref) params.middle
                ++ buildPaginationBtnsTail pag getHref params
            )
        ]


buildPaginationBtnsHead : Pagination a -> (Int -> String) -> PageBtnsParams -> List (Html msg)
buildPaginationBtnsHead pag getHref params =
    [ viewPageBtn "«" (getHref (pag.number + 1)) False pag.first
    , maybeRender params.hasFirstBtn (viewPageBtn "1" (getHref pag.totalPages) (pag.number == 0) False)
    , maybeRender params.hasFirstEllipsis (viewPageBtn "..." "" False True)
    ]


buildPaginationBtnsTail : Pagination a -> (Int -> String) -> PageBtnsParams -> List (Html msg)
buildPaginationBtnsTail pag getHref params =
    [ maybeRender params.hasLastEllipsis (viewPageBtn "..." "" False True)
    , maybeRender params.hasLastBtn (viewPageBtn (String.fromInt pag.totalPages) (getHref pag.totalPages) (pag.number == 0) False)
    , viewPageBtn "»" (getHref (pag.number + 3)) False pag.last
    ]


viewPageBtn : String -> String -> Bool -> Bool -> Html msg
viewPageBtn label pageHref active disabled =
    li
        [ class "page-item"
        , classList [ ( "active", active ), ( "disabled", disabled ) ]
        ]
        [ a
            [ class "page-link"
            , href pageHref
            ]
            [ text label ]
        ]


mapPageBtn : Pagination a -> (Int -> String) -> Int -> Html msg
mapPageBtn pag getHref page =
    viewPageBtn (String.fromInt page) (getHref (page + 1)) ((pag.number + 1) == page) False


pageParser : Query.Parser Int
pageParser =
    Query.map (Maybe.withDefault 1) (Query.int "page")
