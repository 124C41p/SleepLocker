module Helpers exposing (responseDecoder, expectResponse, delay, raidUserKeyEncoder)
import Http
import Process
import Task
import Json.Decode as Decode exposing(Decoder)
import Json.Encode as Encode

responseDecoder : Decoder a -> Decoder (Result String a)
responseDecoder decoder =
    Decode.field "success" Decode.bool
    |> Decode.andThen
        ( \success -> 
            if success then
                Decode.field "result" decoder
                |> Decode.map Ok
            else
                Decode.field "errorMsg" Decode.string
                |> Decode.map Err
        )
            
expectResponse : (Result String a -> msg) -> Maybe msg -> Decoder a -> Http.Expect msg
expectResponse converter defaultMsg decoder =
    Http.expectJson
        ( Result.map converter >> Result.withDefault (Maybe.withDefault (converter <| Err "Unbekannter Fehler. Versuche es später nochmal.") defaultMsg) )
        ( responseDecoder decoder )

delay : Float -> msg -> Cmd msg
delay time msg =
    Process.sleep time
    |> Task.perform (\() -> msg)
    
raidUserKeyEncoder : String -> Encode.Value
raidUserKeyEncoder raidID =
    Encode.object [ ( "raidUserKey", Encode.string raidID ) ]