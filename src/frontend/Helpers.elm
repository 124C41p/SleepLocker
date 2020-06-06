module Helpers exposing (responseDecoder, expectResponse, delay, raidUserKeyEncoder, focus, dateString, timeString, loadTimeZone, viewTitle)
import Http
import Process
import Task
import Json.Decode as Decode exposing(Decoder)
import Json.Encode as Encode
import Browser.Dom as Dom
import Time exposing (Posix, Zone, Month, toHour, toMinute, toSecond, toYear, toMonth, toDay)
import String exposing (String)
import Html exposing (Html, text, div, p, h4)
import Html.Attributes exposing (class)

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

monthString : Month -> String
monthString month =
    case month of
        Time.Jan -> "01"
        Time.Feb -> "02"
        Time.Mar -> "03"
        Time.Apr -> "04"
        Time.May -> "05"
        Time.Jun -> "06"
        Time.Jul -> "07"
        Time.Aug -> "08"
        Time.Sep -> "09"
        Time.Oct -> "10"
        Time.Nov -> "11"
        Time.Dec -> "12"

dateString : Zone -> Posix -> String
dateString zone time =
    String.padLeft 2 '0' (String.fromInt (toDay zone time))
    ++ "." ++
    monthString (toMonth zone time)
    ++ "." ++
    String.fromInt (toYear zone time)

timeString : Zone -> Posix -> String
timeString zone time =
    String.padLeft 2 '0' (String.fromInt (toHour zone time))
    ++ ":" ++
    String.padLeft 2 '0' (String.fromInt (toMinute zone time))
    ++ ":" ++
    String.padLeft 2 '0' (String.fromInt (toSecond zone time))

loadTimeZone : (Zone -> msg) -> Cmd msg
loadTimeZone f = Task.perform f Time.here

viewTitle : Zone -> String -> Posix -> Html msg
viewTitle zone title time =
    div [ class "text-center" ]
        [ h4 [ class "mb-0" ] [ text title ]
        , p [ class "small" ]
            [ text <|
                "‒ erstellt am " ++ dateString zone time ++ " ‒"
            ]
        ]