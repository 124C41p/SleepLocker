module Helpers exposing (responseDecoder, expectResponse, delay, raidUserKeyEncoder, focus)
import Http
import Process
import Task
import Json.Decode as Decode exposing(Decoder)
import Json.Encode as Encode
import Browser.Dom as Dom

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

focus : String -> msg -> Cmd msg
focus id msg =
    Task.attempt (\_ -> msg) (Dom.focus id)
    
raidUserKeyEncoder : String -> Encode.Value
raidUserKeyEncoder raidID =
    Encode.object [ ( "raidUserKey", Encode.string raidID ) ]