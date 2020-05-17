module Register exposing (main)
import Html exposing (Html, div, h4, text, label, input, button, span)
import Html.Attributes exposing (class, for, value, id, attribute, style, disabled)
import Html.Events exposing (onInput, onClick)
import Http
import Process
import Task
import Browser
import NiceSelect exposing (niceSelect, option, optionGroup, selectedValue, searchable, nullable, onUpdate)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode

main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
    }

type alias Flags = Environment

type alias Model = 
    { state : State
    , env : Environment
    }
type State
    = Loading
    | DisplayingStoredData UserData
    | EditingPartialData PartialUserData
    | EditingCompleteData
        { userData : UserData
        , errorMsg : Maybe String
        }

type alias Environment = 
    { lootTable : Maybe (List ItemLocation)
    , classDescriptions : List ClassDescription
    }

type alias ClassDescription =
    { className : String
    , specializations : List String
    }
    
getSpecializations : String -> List ClassDescription -> Maybe (List String)
getSpecializations className =
    List.filter (\d -> d.className == className)
        >> List.head
        >> Maybe.map (\d -> d.specializations)

type alias ItemLocation =
    { locationName : String
    , items : List String        
    }

type alias UserData =
    { userName : String
    , class : String
    , specialization : String
    , prio1 : Maybe String
    , prio2 : Maybe String
    }

userDataEncoder : UserData -> Encode.Value
userDataEncoder data =
    Encode.object
        [ ( "userName", Encode.string data.userName )
        , ( "class", Encode.string data.class )
        , ( "specialization", Encode.string data.specialization )
        , ( "prio1", Maybe.withDefault Encode.null <| Maybe.map Encode.string data.prio1 )
        , ( "prio2", Maybe.withDefault Encode.null <| Maybe.map Encode.string data.prio2 )
        ]

userDataDecoder : Decoder UserData
userDataDecoder =
    Decode.map5 UserData
        (Decode.field "userName" Decode.string)
        (Decode.field "class" Decode.string)
        (Decode.field "specialization" Decode.string)
        (Decode.field "prio1" (Decode.nullable Decode.string))
        (Decode.field "prio2" (Decode.nullable Decode.string))

responseDecoder : Decoder a -> Decoder (Result String a)
responseDecoder decoder =
    Decode.field "success" Decode.bool
        |> Decode.andThen
            ( \success ->  if success then
                Decode.field "result" decoder
                    |> Decode.map Ok
             else
                Decode.field "errorMsg" Decode.string
                    |> Decode.map Err
            )
            
expectResponse : Maybe msg -> (Result String a -> msg) -> Decoder a -> Http.Expect msg
expectResponse defaultMsg converter decoder =
    Http.expectJson
        ( Result.map converter >> Result.withDefault (Maybe.withDefault (converter <| Err "Interner Serverfehler.") defaultMsg) )
        ( responseDecoder decoder )

type alias PartialUserData =
    { userName : String
    , class : Maybe PartialClass
    , prio1 : Maybe String
    , prio2 : Maybe String
    }

type alias PartialClass =
    { className : String
    , specialization : Maybe String
    }

emptyUserData : PartialUserData
emptyUserData = PartialUserData "" Nothing Nothing Nothing

updateClass : (PartialClass -> Maybe PartialClass) -> PartialUserData -> PartialUserData
updateClass fun data = { data | class = Maybe.andThen fun data.class }

validateUserData : PartialUserData -> Maybe UserData
validateUserData data =
    Maybe.map3 UserData
        (let
            len = String.length data.userName
        in
            if len == 0 || len > 50 then Nothing else Just data.userName)
        (Maybe.map (\class -> class.className) data.class)
        (Maybe.andThen (\class -> class.specialization) data.class)
    |> Maybe.map (\partialData -> partialData data.prio1 data.prio2)

invalidateUserData : UserData -> PartialUserData
invalidateUserData data =
    PartialUserData
        data.userName
        (Just <| PartialClass data.class (Just data.specialization))
        data.prio1
        data.prio2

buildState : PartialUserData -> State
buildState data =
    case validateUserData data of
        Nothing -> EditingPartialData data
        Just completeData -> EditingCompleteData { userData = completeData, errorMsg = Nothing }

loadUserData : Cmd Msg
loadUserData =
    Http.get
        { url = "/api/myData"
        , expect = expectResponse (Just WaitAndReload)
            ( Result.withDefault UnknownUser << Result.map DataLoaded )
            userDataDecoder
        }

storeUserData : UserData -> Cmd Msg
storeUserData data =
    Http.post
        { url = "/api/register"
        , body = Http.jsonBody (userDataEncoder data)
        , expect =
            expectResponse Nothing
            ( DataStored data )
            ( Decode.null () )
        }

clearUserData : UserData -> Cmd Msg
clearUserData data =
    Http.get
        { url = "/api/clearMyData"
        , expect =
            expectResponse Nothing
                ( \result ->
                    case result of
                        Err _ -> DataStored data result
                        Ok () -> DataUpdated (invalidateUserData data)
                )
                ( Decode.null () )
        }

init : Flags -> (Model, Cmd Msg)
init env =
    (
        { state = Loading
        , env = env
        }
    , loadUserData
    )

type Msg
    = Reload
    | WaitAndReload
    | UnknownUser
    | DataLoaded UserData
    | DataUpdated PartialUserData
    | DataStored UserData (Result String ())
    | RegisterUserData UserData
    | ClearUserData UserData

delay : Float -> msg -> Cmd msg
delay time msg =
    Process.sleep time
    |> Task.perform (\() -> msg)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Reload ->
            ( { model | state = Loading }
            , loadUserData
            )
        WaitAndReload ->
            ( { model | state = Loading }
            , delay 1000 Reload
            )
        UnknownUser ->
            ( { model | state = EditingPartialData emptyUserData }
            , Cmd.none
            )
        DataLoaded userData ->
            ( { model | state = DisplayingStoredData userData }
            , Cmd.none
            )
        DataUpdated newData ->
            ( { model | state = buildState newData }
            , Cmd.none
            )
        DataStored data result ->
            case result of
                Ok () ->
                    ( { model | state = Loading }
                    , loadUserData
                    )
                Err errMsg ->
                    ( { model | state = EditingCompleteData { userData = data, errorMsg = Just errMsg } }
                    , Cmd.none
                    )
        RegisterUserData data ->
            ( { model | state = Loading }
            , storeUserData data
            )
        ClearUserData data ->
            ( { model | state = Loading }
            , clearUserData data
            )

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none

view : Model -> Html Msg
view model = viewFormBorder <|
    case model.state of
        Loading -> viewLoading
        EditingPartialData data -> viewFormPartial model.env data
        DisplayingStoredData userData -> viewFormLocked model.env userData
        EditingCompleteData { userData, errorMsg } -> viewFormComplete model.env userData errorMsg

viewLoading : Html Msg
viewLoading =
    div [ class "d-flex", class "justify-content-center" ]
        [ div [ class "spinner-border", class "text-primary", attribute "role" "status",
                style "width" "3rem", style "height" "3rem", class "mt-5", class "mb-5" ]
            [ span [ class "sr-only" ] [ text "Lade..." ]
            ]
        ]

viewFormBorder : Html Msg -> Html Msg
viewFormBorder innerHtml =
    div [ class "row", class "padding", class "d-flex", class "justify-content-center", class "mt-5" ]
    [ div [ class "col-md-5" ]
        [ div [ class "card" ]
            [ div [ class "card-body" ]
                [ h4 [ class "card-title", class "text-center", class "padding" ] [ text "Softlocks registrieren" ]
                , div [ class "padding" ] []
                , innerHtml
                ]
            ]
        ]
    ]

viewFormPartial : Environment -> PartialUserData -> Html Msg
viewFormPartial env data =
    div []
        [ viewInputForm env data False
        , div [ class "btn-group" ]
            [ button [ class "btn", class "btn-secondary", disabled True ] [ text "Abschicken" ]
            , button [ class "btn", class "btn-secondary", disabled True ] [ text "Stornieren" ]
            ]
        ]
        
viewFormComplete : Environment -> UserData -> Maybe String -> Html Msg
viewFormComplete env data error =
    div [] <|
        List.concat
            [ Maybe.withDefault [] (Maybe.map (\errMsg -> [ viewAlert errMsg ]) error)
            ,
                [ viewInputForm env (invalidateUserData data) False
                , div [ class "btn-group" ]
                    [ button [ class "btn", class "btn-primary", onClick (RegisterUserData data) ] [ text "Abschicken" ]
                    , button [ class "btn", class "btn-secondary", disabled True ] [ text "Stornieren" ]
                    ]
                ]
            ]

viewAlert : String -> Html Msg
viewAlert message =
    div [ class "alert", class "alert-danger" ]
        [ span [] [ text message ]
        ]

viewSuccess : String -> Html Msg
viewSuccess message =
    div [ class "alert", class "alert-success" ]
        [ span [] [ text message ]
        ]

viewFormLocked : Environment -> UserData -> Html Msg
viewFormLocked env data =
    div []
        [ viewSuccess "Deine Anmeldung wurde gespeichert."
        , viewInputForm env (invalidateUserData data) True
        , div [ class "btn-group" ]
            [ button [ class "btn", class "btn-secondary", disabled True ] [ text "Abschicken" ]
            , button [ class "btn", class "btn-primary", onClick (ClearUserData data) ] [ text "Stornieren" ]
            ]
        ]

viewInputForm : Environment -> PartialUserData -> Bool -> Html Msg
viewInputForm env data isDisabled =
    div []
        [ div [ class "form-group" ]
        [ label [ for "userName" ] [ text "Charaktername" ]
        , input 
            [ class "form-control", id "userName", value data.userName, disabled isDisabled
            , onInput ( \newName -> DataUpdated { data | userName = newName } )
            ]
            [ ]
        ]
    , div [ class "form-row" ]
        [ div [ class "form-group", class "col-md-6" ]
            [ label [ ] [ text "Klasse" ]
            , niceSelect 
                [ selectedValue ( Maybe.map (\c -> c.className) data.class )
                , disabled isDisabled
                , onUpdate 
                    ( \newClass -> 
                        DataUpdated { data | class = Maybe.map (\newName -> PartialClass newName Nothing) newClass }
                    )
                ]
                <| List.map (\d -> option d.className) env.classDescriptions
            ]
        , div [ class "form-group", class "col-md-6" ]
            [ label [ ] [ text "Spezialisierung" ]
            , niceSelect 
                [ selectedValue ( Maybe.andThen (\cls -> cls.specialization) data.class )
                , disabled (isDisabled || data.class == Nothing)
                , onUpdate (\newSpec -> DataUpdated <| updateClass (\cls -> Just { cls | specialization = newSpec }) data )
                ]
                ( Maybe.andThen ( \cls -> getSpecializations cls.className env.classDescriptions ) data.class
                    |> Maybe.withDefault []
                    |> List.map option
                )
            ]
        ]
    , viewInputFormSoftlock env.lootTable "Prio 1" data.prio1 (\newValue -> { data | prio1 = newValue }) isDisabled
    , viewInputFormSoftlock env.lootTable "Prio 2" data.prio2 (\newValue -> { data | prio2 = newValue }) isDisabled
    ]

viewInputFormSoftlock : Maybe (List ItemLocation) -> String -> Maybe String -> (Maybe String -> PartialUserData) -> Bool -> Html Msg
viewInputFormSoftlock loot labelStr itemName updateFun isDisabled =
    div [ class "form-group" ]
            [ label [ ] [ text labelStr ]
            , case loot of
                Nothing ->
                    input 
                        [ class "form-control", id "userName", value (Maybe.withDefault "" itemName), disabled isDisabled
                        , onInput (Just >> updateFun >> DataUpdated)
                        ]
                        [ ]
                Just lootLocations ->
                    niceSelect
                        [ selectedValue itemName, searchable, nullable, disabled isDisabled
                        , onUpdate (updateFun >> DataUpdated)
                        ]
                        <| List.map (\l -> optionGroup l.locationName l.items ) lootLocations
            ]