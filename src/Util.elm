module Util exposing (delay, getLast)

import Process
import Task


getLast : List a -> Maybe a
getLast list =
    List.head (List.reverse list)


delay : Float -> msg -> Cmd msg
delay time msg =
    Process.sleep time
        |> Task.andThen (always <| Task.succeed msg)
        |> Task.perform identity
