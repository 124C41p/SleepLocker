module Register exposing (main)
import Html exposing (Html, div, h4, text, label, input, button )
import Html.Attributes exposing (class, for, value, id)
import Browser
import NiceSelect exposing (niceSelect, option, selectedValue, searchable, nullable)


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
    }

type alias Flags = String

type alias Model =
    { values : List String
    , current : Maybe String
    }


init : Flags -> (Model, Cmd Msg)
init _ =
    ({ values = ["foo", "bar", "baz"], current = Just "baz" }, Cmd.none)


type Msg
    = NoOp


update : Msg -> Model -> (Model, Cmd Msg)
update msg model = (model, Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Html Msg
view model =
    div [ class "row", class "padding", class "d-flex", class "justify-content-center", class "mt-5" ]
    [ div [ class "col-md-5" ]
        [ div [ class "card" ]
            [ div [ class "card-body" ]
                [ h4 [ class "card-title", class "text-center", class "padding" ] [ text "Softlocks registrieren" ]
                , div [ class "padding" ] []
                , registerForm model
                ,  div [ class "btn-group" ]
                    [ button [ class "btn", class "btn-primary" ] [ text "Abschicken" ]
                    , button [ class "btn", class "btn-secondary" ] [ text "Stornieren" ]
                    ]
                ]
            ]
        ]
    ]

registerForm : Model -> Html Msg
registerForm _ =
    div []
    [ div [ class "form-group" ]
        [ label [ for "name" ] [ text "Charaktername" ]
        , input [ class "form-control", id "name", value "Ein Name" ] [ ]
        ]
    , div [ class "form-row" ]
        [ div [ class "form-group", class "col-md-6" ]
            [ label [ ] [ text "Klasse" ]
            , niceSelect [ selectedValue "Schurke" ] [ option "Schurke" ]
            ]
        , div [ class "form-group", class "col-md-6" ]
            [ label [ ] [ text "Spezialisierung" ]
            , niceSelect [ selectedValue "Dolche" ] [ option "Dolche", option "Schwerter" ]
            ]
        ]
    , div [ class "form-group" ]
        [ label [ ] [ text "Prio 1" ]
        , niceSelect [ selectedValue "foo", searchable, nullable ] [ option "foo", option "bar" ]
        ]
    , div [ class "form-group" ]
        [ label [ ] [ text "Prio 2" ]
        , niceSelect [ selectedValue "bar", searchable, nullable ] [ option "foo", option "bar" ]
        ]
    ]