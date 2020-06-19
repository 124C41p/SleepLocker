module Helpers exposing
    ( delay
    , focus
    , dateString
    , timeString
    , loadTimeZone
    , viewTitle
    , viewInitError
    , viewQuestionModal
    , viewModalBackdrop
    )
import Process
import Task
import Browser.Dom as Dom
import Time exposing (Posix, Zone, Month, toHour, toMinute, toSecond, toYear, toMonth, toDay)
import String exposing (String)
import Html exposing (Html, text, div, p, h4, h5, button, span)
import Html.Attributes exposing (class, tabindex, style)
import Html.Events exposing (onClick)

delay : Float -> msg -> Cmd msg
delay time msg =
    Process.sleep time
    |> Task.perform (\() -> msg)

focus : String -> msg -> Cmd msg
focus id msg =
    Task.attempt (\_ -> msg) (Dom.focus id)
    
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

viewInitError : String -> Html msg
viewInitError err =
    div [ class "mt-5", class "p-1" ]
        [ h4 [] [ text "Interner Fehler" ]
        , p [ class "small" ]
            [ text err
            ]
        ]

viewQuestionModal : String -> List (Html msg) -> msg -> msg -> Html msg
viewQuestionModal title innerHtml proceed cancel =
    div [ class "modal", tabindex -1, style "display" "block" ]
        [ div [class "modal-dialog", class "modal-dialog-centered" ]
            [ div [ class "modal-content" ]
                [ div [ class "modal-header" ]
                    [ h5 [ class "modal-title" ] [ text title ]
                    , button [ class "close", onClick cancel ]
                        [ span [] [ text "×" ]
                        ]
                    ]
                , div [ class "modal-body" ] innerHtml
                , div [ class "modal-footer" ]
                    [ div [ class "btn-group" ]
                        [ button [ class "btn", class "btn-primary", onClick proceed ] [ text "Ja" ]
                        , button [ class "btn", class "btn-secondary", onClick cancel ] [ text "Nein" ]
                        ]
                    ]
                ]
            ]
        ]

viewModalBackdrop : Html msg
viewModalBackdrop =
    div [ class "modal-backdrop", class "show" ] []