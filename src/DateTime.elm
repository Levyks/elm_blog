module DateTime exposing (viewTimeAgo)

import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (attribute)
import Time exposing (Month(..), Posix, Zone, toDay, toHour, toMinute, toMonth, toSecond, toYear, utc)


toISO8601 : Time.Posix -> String
toISO8601 time =
    getYearString utc time
        ++ "-"
        ++ getMonthNumString utc time
        ++ "-"
        ++ getDayNumString utc time
        ++ "T"
        ++ getHourNumString utc time
        ++ ":"
        ++ getMinuteNumString utc time
        ++ ":"
        ++ getSecondNumString utc time
        ++ ".000+00:00"


viewTimeAgo : Time.Posix -> Html msg
viewTimeAgo time =
    let
        iso8601 =
            toISO8601 time
    in
    node "time-ago"
        [ attribute "datetime" iso8601
        ]
        [ text iso8601 ]


getDayNumString : Zone -> Posix -> String
getDayNumString zone time =
    let
        day =
            String.fromInt (toDay zone time)
    in
    if String.length day == 1 then
        "0" ++ day

    else
        day


getMonthNumString : Zone -> Posix -> String
getMonthNumString zone time =
    case toMonth zone time of
        Jan ->
            "01"

        Feb ->
            "02"

        Mar ->
            "03"

        Apr ->
            "04"

        May ->
            "05"

        Jun ->
            "06"

        Jul ->
            "07"

        Aug ->
            "08"

        Sep ->
            "09"

        Oct ->
            "10"

        Nov ->
            "11"

        Dec ->
            "12"


getYearString : Zone -> Posix -> String
getYearString zone time =
    String.fromInt (toYear zone time)


getHourNumString : Zone -> Posix -> String
getHourNumString zone time =
    let
        hour =
            String.fromInt (toHour zone time)
    in
    if String.length hour == 1 then
        "0" ++ hour

    else
        hour


getMinuteNumString : Zone -> Posix -> String
getMinuteNumString zone time =
    let
        minute =
            String.fromInt (toMinute zone time)
    in
    if String.length minute == 1 then
        "0" ++ minute

    else
        minute


getSecondNumString : Zone -> Posix -> String
getSecondNumString zone time =
    let
        second =
            String.fromInt (toSecond zone time)
    in
    if String.length second == 1 then
        "0" ++ second

    else
        second
