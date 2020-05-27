module Admin exposing (main)

import Html exposing (Html, div, text, span, button, table, tr, th, thead, tbody, td, input, label)
import Html.Attributes exposing (class, scope, value, disabled, attribute)
import Html.Events exposing (onClick)
import Http
import Browser
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Time
import Helpers exposing (expectResponse)
import Result.Extra as ResultX


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
    }

type InfoMessage
    = NoMsg
    | SuccessMsg String
    | ErrorMsg String

type alias Model =
    { raidAdminKey : String
    , raidUserKey : String
    , userList : List UserData
    , currentMessage : InfoMessage
    , isLoading : Bool
    }

type alias UserData =
    { userName : String
    , class : String
    , role : String
    , registeredOn : String
    }

userDataDecoder : Decoder UserData
userDataDecoder =
    Decode.map4 UserData
        (Decode.field "userName" Decode.string)
        (Decode.field "class" Decode.string)
        (Decode.field "role" Decode.string)
        (Decode.field "registeredOn" Decode.string)

type alias Flags =
    { raidAdminKey : String
    , raidUserKey : String
    }

type Mode
    = Register
    | ShowTables

getModeNumber : Mode -> Int
getModeNumber mode =
    case mode of
        Register -> 0
        ShowTables -> 1

init : Flags -> (Model, Cmd Msg)
init { raidAdminKey, raidUserKey } =
    ( { raidAdminKey = raidAdminKey, raidUserKey = raidUserKey, userList = [], currentMessage = NoMsg, isLoading = False }
    , loadRegistrations raidAdminKey
    )

type Msg
    = NoOp
    | DoUpdate
    | Updated (List UserData)
    | DoSetMode Mode
    | ModeSet InfoMessage


loadRegistrations : String -> Cmd Msg
loadRegistrations raidAdminKey =
    Http.post
        { url = "/api/getRegistrations"
        , body =
            Http.jsonBody
            <| Encode.object
                [ ( "raidAdminKey", Encode.string raidAdminKey )
                ]
        , expect = expectResponse
            ( ResultX.unwrap NoOp Updated )
            Nothing
            (Decode.list userDataDecoder)
        }

setMode : String -> Mode -> Cmd Msg
setMode raidAdminKey mode =
    Http.post
        { url = "/api/setMode"
        , body =
            Http.jsonBody
            <| Encode.object
                [ ( "raidAdminKey", Encode.string raidAdminKey )
                , ( "mode", Encode.int <| getModeNumber mode )
                ]
        , expect = expectResponse
            ( \res -> case res of
                Err errMsg -> ModeSet <| ErrorMsg errMsg
                Ok () -> ModeSet <| SuccessMsg "Modus erfolgreich gesetzt"
            )
            Nothing
            (Decode.null ())
        }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NoOp -> (model, Cmd.none)
        DoUpdate -> (model, loadRegistrations model.raidAdminKey)
        Updated userList -> ({ model | userList = userList }, Cmd.none)
        DoSetMode mode -> ({ model | currentMessage = NoMsg, isLoading = True }, setMode model.raidAdminKey mode )
        ModeSet res -> ({ model | currentMessage = res, isLoading = False }, Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 5000 (\_ -> DoUpdate)


view : Model -> Html Msg
view model =
    div [ class "container-fluid", class "pb-5" ]
        [ div [ class "row" ]
            [ div [ class "col-md-2" ] [ viewControls model.raidUserKey model.currentMessage model.isLoading ]
            , div [ class "col-md-10" ] [ viewUserList model.userList ]
            ]
        ]

viewControls : String -> InfoMessage -> Bool -> Html Msg
viewControls raidUserKey message isLoading =
    div [ class "card" ]
        [ div [ class "card-body" ] 
            [ Maybe.withDefault (div [] []) (viewInfoMsg message)
            , div [ class "form-group" ]
                [ label [] [ text "Raid ID" ]
                , input [ class "form-control", disabled True, value raidUserKey ] []
                ]
            ,
                if isLoading then
                    viewSpinner
                else
                    viewButtons
            ]
        ]

viewButtons : Html Msg
viewButtons = 
    div [ class "btn-group-vertical", class "d-flex" ]
        [ button
            [ class "btn"
            , class "btn-outline-primary"
            , onClick (DoSetMode Register)
            ] 
            [ text "Registrieren" ]
        , button
            [ class "btn"
            , class "btn-outline-primary"
            , onClick (DoSetMode ShowTables)
            ]
            [ text "Veröffentlichen" ]
        ]

viewUserList : List UserData -> Html Msg
viewUserList userList =
    table [ class "table", class "table-striped", class "table-bordered" ]
        [ thead []
            [ th [ scope "col" ] [ text "#" ]
            , th [ scope "col" ] [ text "Name" ]
            , th [ scope "col" ] [ text "Klasse" ]
            , th [ scope "col" ] [ text "Rolle" ]
            , th [ scope "col" ] [ text "Anmeldezeitpunkt" ]
            ]
        , tbody [] ( List.indexedMap viewUserRow userList )
        ]
 
viewUserRow : Int -> UserData -> Html Msg
viewUserRow index userData =
    tr []
        [ th [ scope "row" ] [ text ( String.fromInt (index + 1) ) ]
        , td [] [ text userData.userName ]
        , td [] [ text userData.class ]
        , td [] [ text userData.role ]
        , td [] [ text userData.registeredOn ]
        ]

viewInfoMsg : InfoMessage -> Maybe (Html Msg)
viewInfoMsg infoMsg =
    case infoMsg of
        NoMsg -> Nothing
        ErrorMsg message -> Just <|
            div [ class "alert", class "alert-danger" ]
                [ span [] [ text message ]
                ]
        SuccessMsg message -> Just <|
            div [ class "alert", class "alert-success" ]
                [ span [] [ text message ]
                ]

viewSpinner : Html Msg
viewSpinner =
    div [ class "d-flex", class "justify-content-center" ]
        [ div [ class "spinner-border", class "text-primary", attribute "role" "status" ]
            [ span [ class "sr-only" ] [ text "Lade..." ] ]
        ]