module Admin exposing (main)

import Html exposing (Html, div, text, span, button, h4, table, tr, th, thead, tbody, td)
import Html.Attributes exposing (class, scope)
import Html.Events exposing (onClick)
import Http
import Browser
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Time
import Helpers exposing (expectResponse)


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
    { adminKey : String
    , userList : List UserData
    , currentMessage : InfoMessage
    }

type alias UserData =
    { userName : String
    , class : String
    , role : String
    }

userDataDecoder : Decoder UserData
userDataDecoder =
    Decode.map3 UserData
        (Decode.field "userName" Decode.string)
        (Decode.field "class" Decode.string)
        (Decode.field "role" Decode.string)

type alias Flags = String

type Mode
    = Ready
    | Register
    | ShowTables
    | Closed

getModeNumber : Mode -> Int
getModeNumber mode =
    case mode of
        Ready -> 0
        Register -> 1
        ShowTables -> 2
        Closed -> 3

getModeName : Mode -> String
getModeName mode =
    case mode of
        Ready -> "On Hold"
        Register -> "Registrieren"
        ShowTables -> "Tabellen"
        Closed -> "Abschließen"

init : Flags -> (Model, Cmd Msg)
init adminKey =
    ( { adminKey = adminKey, userList = [], currentMessage = NoMsg }
    , loadRegistrations adminKey
    )

type Msg
    = NoOp
    | DoUpdate
    | Updated (List UserData)
    | DoSetMode Mode
    | ModeSet InfoMessage


loadRegistrations : String -> Cmd Msg
loadRegistrations adminKey =
    Http.post
        { url = "/api/getRegistrations"
        , body =
            Http.jsonBody
            <| Encode.object
                [ ( "adminKey", Encode.string adminKey )
                ]
        , expect = expectResponse
            ( Result.map Updated >> Result.withDefault NoOp )
            Nothing
            (Decode.list userDataDecoder)
        }

setMode : String -> Mode -> Cmd Msg
setMode adminKey mode =
    Http.post
        { url = "/api/setMode"
        , body =
            Http.jsonBody
            <| Encode.object
                [ ( "adminKey", Encode.string adminKey )
                , ( "mode", Encode.int <| getModeNumber mode )
                ]
        , expect = expectResponse
            ( \res -> case res of
                Err errMsg -> ModeSet <| ErrorMsg errMsg
                Ok () -> ModeSet <| SuccessMsg ("Modus \"" ++ getModeName mode ++ "\" erfolgreich gesetzt")
            )
            Nothing
            (Decode.null ())
        }


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NoOp -> (model, Cmd.none)
        DoUpdate -> (model, loadRegistrations model.adminKey)
        Updated userList -> ({ model | userList = userList }, Cmd.none)
        DoSetMode mode -> ({ model | currentMessage = NoMsg }, setMode model.adminKey mode )
        ModeSet res -> ({ model | currentMessage = res }, Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 5000 (\_ -> DoUpdate)


view : Model -> Html Msg
view { userList, currentMessage } =
    div [ class "row", class "padding", class "d-flex", class "justify-content-center", class "mt-11" ]
        [ div [ class "col-md-2" ]
            [ div [ class "card" ]
                [ div [ class "card-body" ] <| List.concat
                    [ [ h4 [ class "card-title", class "text-center", class "padding" ] [ text "Modi" ] ]
                    , viewInfoMsg currentMessage |> Maybe.map List.singleton |> Maybe.withDefault []
                    , 
                        [ button [ class "btn", class "btn-primary", class "mb-1", class "mr-1", onClick (DoSetMode Ready) ] [ text "On Hold" ]
                        , button [ class "btn", class "btn-primary", class "mb-1", class "mr-1", onClick (DoSetMode Register) ] [ text "Registrieren" ]
                        , button [ class "btn", class "btn-primary", class "mb-1", class "mr-1", onClick (DoSetMode ShowTables) ] [ text "Zeige Tabelle" ]
                        , button [ class "btn", class "btn-primary", class "mb-1", class "mr-1", onClick (DoSetMode Closed) ] [ text "Abschließen" ]
                        ]
                    ]
                ]
            ]
        , div [ class "col-md-10" ] [ viewUserList userList ]
        ]

viewUserList : List UserData -> Html Msg
viewUserList userList =
    div [ class "row", class "padding", class "d-flex", class "justify-content-center", class "mt-5" ]
    [ div [ class "col-md-11" ]
        [ table [ class "table", class "table-striped", class "table-bordered" ]
            [ thead []
                [ th [ scope "col" ] [ text "#" ]
                , th [ scope "col" ] [ text "Name" ]
                , th [ scope "col" ] [ text "Klasse" ]
                , th [ scope "col" ] [ text "Rolle" ]
                ]
            , tbody [] ( List.indexedMap viewUserRow userList )
            ]
        ]
    ]
 
viewUserRow : Int -> UserData -> Html Msg
viewUserRow index userData =
    tr []
        [ th [ scope "row" ] [ text ( String.fromInt (index + 1) ) ]
        , td [] [ text userData.userName ]
        , td [] [ text userData.class ]
        , td [] [ text userData.role ]
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