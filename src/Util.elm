module Util exposing (delay, find, getDataFromRemoteData, getLast)

import Process
import RemoteData exposing (RemoteData)
import Task


getLast : List a -> Maybe a
getLast list =
    List.head (List.reverse list)


delay : Float -> msg -> Cmd msg
delay time msg =
    Process.sleep time
        |> Task.andThen (always <| Task.succeed msg)
        |> Task.perform identity


find : (a -> Bool) -> List a -> Maybe a
find f list =
    List.head (List.filter f list)


getDataFromRemoteData : RemoteData e a -> Maybe a
getDataFromRemoteData data =
    case data of
        RemoteData.Success a ->
            Just a

        _ ->
            Nothing
