module Register exposing (main)
import Html exposing (Html, div, h4, text, label, input, button, span, hr)
import Html.Attributes exposing (class, for, value, id, attribute, style, disabled)
import Html.Events exposing (onInput, onClick)
import Http
import Browser
import NiceSelect exposing (niceSelect, option, optionGroup, selectedValue, searchable, nullable, onUpdate)
import Json.Decode as Decode
import Json.Encode as Encode
import UserData exposing (UserData, userDataDecoder, userDataEncoder)
import Helpers exposing (expectResponse, delay, loadTimeZone, viewTitle)
import Markdown
import Markdown.Config as MDConfig exposing (defaultOptions)
import Maybe.Extra as MaybeX
import Time exposing (Posix, Zone, millisToPosix)
import Iso8601 exposing (toTime)

main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
    }

type alias Flags = 
    { lootTable : Maybe (List ItemLocation)
    , classDescriptions : List ClassDescription
    , raidID : String
    , userID : String
    , comments : Maybe String
    , title : String
    , createdOn : String
    }

type alias Model = 
    { state : State
    , env : Environment
    , timeZone : Zone
    }
type State
    = Loading
    | DisplayingStoredData
        { userData : UserData
        , infoMessage : InfoMessage
        }
    | EditingPartialData PartialUserData
    | EditingCompleteData
        { userData : UserData
        , infoMessage : InfoMessage
        }

type InfoMessage
    = NoMsg
    | SuccessMsg String
    | ErrorMsg String

type alias Environment = 
    { lootTable : Maybe (List ItemLocation)
    , classDescriptions : List ClassDescription
    , raidID : String
    , userID : String
    , comments : Maybe (Html Msg)
    , title : String
    , createdOn : Posix
    }

type alias ClassDescription =
    { className : String
    , roles : List String
    }
    
getRoles : String -> List ClassDescription -> Maybe (List String)
getRoles className =
    List.filter (\d -> d.className == className)
        >> List.head
        >> Maybe.map (\d -> d.roles)

type alias ItemLocation =
    { locationName : String
    , loot : List String        
    }

type alias PartialUserData =
    { userName : String
    , class : Maybe PartialClass
    , prio1 : Maybe String
    , prio2 : Maybe String
    }

type alias PartialClass =
    { className : String
    , role : Maybe String
    }

emptyUserData : PartialUserData
emptyUserData = PartialUserData "" Nothing Nothing Nothing

updateClass : (PartialClass -> Maybe PartialClass) -> PartialUserData -> PartialUserData
updateClass fun data = { data | class = Maybe.andThen fun data.class }

partialClass : List ClassDescription -> String -> PartialClass
partialClass classes newName =
    let
        newRole = 
            List.filter (\c -> c.className == newName) classes
            |> List.head
            |> Maybe.andThen
                ( \c ->  if List.length c.roles == 1 then
                    List.head c.roles
                else
                    Nothing
                )
    in PartialClass newName newRole   

validateUserData : PartialUserData -> Maybe UserData
validateUserData data =
    Maybe.map3 UserData
        (let
            len = String.length data.userName
        in
            if len == 0 || len > 50 then Nothing else Just data.userName)
        (Maybe.map (\class -> class.className) data.class)
        (Maybe.andThen (\class -> class.role) data.class)
    |> Maybe.map (\partialData -> partialData data.prio1 data.prio2)

invalidateUserData : UserData -> PartialUserData
invalidateUserData data =
    PartialUserData
        data.userName
        (Just <| PartialClass data.class (Just data.role))
        data.prio1
        data.prio2

buildState : PartialUserData -> State
buildState data =
    case validateUserData data of
        Nothing -> EditingPartialData data
        Just completeData -> EditingCompleteData { userData = completeData, infoMessage = NoMsg }

userQueryEncoder : String -> String -> Encode.Value
userQueryEncoder raidID userID =
    Encode.object
        [ ( "raidUserKey", Encode.string raidID )
        , ( "userID", Encode.string userID )
        ]

loadUserData : String -> String -> InfoMessage -> Cmd Msg
loadUserData raidID userID infoMsg =
    Http.post
        { url = "/api/myData"
        , body = Http.jsonBody (userQueryEncoder raidID userID)
        , expect = expectResponse
            ( Result.map (DisplayLockedData infoMsg) >> Result.withDefault DisplayEmptyData )
            (Just <| WaitAndReload infoMsg )
            userDataDecoder
        }

storeUserData : String -> String -> UserData -> Cmd Msg
storeUserData raidID userID data =
    Http.post
        { url = "/api/register"
        , body = Http.jsonBody (userDataEncoder raidID userID data)
        , expect =
            expectResponse
            ( \res -> case res of
                Err message -> DisplayEditableData (ErrorMsg message) data
                Ok () -> Reload (SuccessMsg "Du bist jetzt angemeldet.")
            )
            Nothing
            ( Decode.null () )
        }

clearUserData : String -> String -> UserData -> Cmd Msg
clearUserData raidID userID data =
    Http.post
        { url = "/api/clearMyData"
        , body = Http.jsonBody (userQueryEncoder raidID userID)
        , expect =
            expectResponse
                ( \result ->
                    case result of
                        Err errorMsg -> Reload (ErrorMsg errorMsg)
                        Ok () -> DisplayEditableData (SuccessMsg "Deine Anmeldung wurde storniert.") data
                )
                Nothing
                ( Decode.null () )
        }

init : Flags -> (Model, Cmd Msg)
init flags =
    (
        { state = Loading
        , timeZone = Time.utc
        , env =
            { lootTable = flags.lootTable
            , classDescriptions = flags.classDescriptions
            , raidID = flags.raidID
            , userID = flags.userID
            , comments =
                Maybe.map
                    ( div [] << 
                        ( Markdown.toHtml <|
                            Just { defaultOptions | rawHtml = MDConfig.DontParse }
                        )
                    )
                    flags.comments
            , title = flags.title
            , createdOn =
                case toTime flags.createdOn of
                   Ok time -> time
                   Err _ -> millisToPosix 0
            }
        }
    , Cmd.batch
        [ loadUserData flags.raidID flags.userID NoMsg
        , loadTimeZone NewTimeZone
        ]
    )

type Msg
    = Reload InfoMessage
    | WaitAndReload InfoMessage
    | DisplayEmptyData
    | DisplayEditableData InfoMessage UserData
    | DisplayLockedData InfoMessage UserData
    | DisplayPartialData PartialUserData
    | RegisterUserData UserData
    | CancelUserData UserData
    | NewTimeZone Zone

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Reload infoMsg ->
            ( { model | state = Loading }
            , loadUserData model.env.raidID model.env.userID infoMsg
            )
        WaitAndReload infoMsg ->
            ( { model | state = Loading }
            , delay 1000 (Reload infoMsg)
            )
        DisplayEmptyData ->
            ( { model | state = EditingPartialData emptyUserData }
            , Cmd.none
            )
        DisplayLockedData infoMsg userData ->
            ( { model | state = DisplayingStoredData { userData = userData, infoMessage = infoMsg } }
            , Cmd.none
            )
        DisplayPartialData newData ->
            ( { model | state = buildState newData }
            , Cmd.none
            )
        RegisterUserData data ->
            ( { model | state = Loading }
            , storeUserData model.env.raidID model.env.userID data
            )
        CancelUserData data ->
            ( { model | state = Loading }
            , clearUserData model.env.raidID model.env.userID data
            )
        DisplayEditableData infoMsg userData ->
            ( { model | state = EditingCompleteData { userData = userData, infoMessage = infoMsg } }
            , Cmd.none
            )
        NewTimeZone zone ->
            ( { model | timeZone = zone }
            , Cmd.none
            )

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none

view : Model -> Html Msg
view model = viewFormBorder model.env model.timeZone <|
    case model.state of
        Loading -> viewLoading
        EditingPartialData data -> viewFormPartial model.env data
        DisplayingStoredData { userData, infoMessage } -> viewFormLocked model.env userData infoMessage
        EditingCompleteData { userData, infoMessage } -> viewFormComplete model.env userData infoMessage

viewLoading : Html Msg
viewLoading =
    div [ class "d-flex", class "justify-content-center" ]
        [ div [ class "spinner-border", class "text-primary", attribute "role" "status",
                style "width" "3rem", style "height" "3rem", class "mt-5", class "mb-5" ]
            [ span [ class "sr-only" ] [ text "Lade..." ]
            ]
        ]

viewFormBorder : Environment -> Zone -> Html Msg -> Html Msg
viewFormBorder env zone innerHtml =
    div [ class "container", class "py-5" ]
        [ div [ class "row" ]
            [ div [ class "col" ]
                [ viewTitle zone env.title env.createdOn
                ]
            ]
        , div [ class "row", class "d-flex", class "justify-content-center", class "mt-2" ]
            [ div [ class "col-md-8" ]
                [ div [ class "card" ]
                    [ div [ class "card-body" ]
                        [ h4 [ class "card-title", class "text-center" ] [ text "Anmeldung" ]
                        , MaybeX.unwrap (div [] []) viewComments env.comments
                        , innerHtml
                        ]
                    ]
                ]
            ]
        ]

viewComments : Html msg -> Html msg
viewComments comments =
    div []
        [ hr [] []
        , comments
        , hr [] []
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
        
viewFormComplete : Environment -> UserData -> InfoMessage -> Html Msg
viewFormComplete env data infoMsg =
    div [] <|
        List.concat
            [ Maybe.withDefault [] (Maybe.map List.singleton <| viewInfoMsg infoMsg)
            ,
                [ viewInputForm env (invalidateUserData data) False
                , div [ class "btn-group" ]
                    [ button [ class "btn", class "btn-primary", onClick (RegisterUserData data) ] [ text "Abschicken" ]
                    , button [ class "btn", class "btn-secondary", disabled True ] [ text "Stornieren" ]
                    ]
                ]
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

viewFormLocked : Environment -> UserData -> InfoMessage -> Html Msg
viewFormLocked env data infoMsg =
    div [] <|
        List.concat
            [ Maybe.withDefault [] (Maybe.map List.singleton <| viewInfoMsg infoMsg)
            ,
                [ viewInputForm env (invalidateUserData data) True
                , div [ class "btn-group" ]
                    [ button [ class "btn", class "btn-secondary", disabled True ] [ text "Abschicken" ]
                    , button [ class "btn", class "btn-primary", onClick (CancelUserData data) ] [ text "Stornieren" ]
                    ]
                ]
            ]

viewInputForm : Environment -> PartialUserData -> Bool -> Html Msg
viewInputForm env data isDisabled =
    div []
        [ div [ class "form-group" ]
            [ label [ for "userName" ] [ text "Charaktername" ]
            , input 
                [ class "form-control", id "userName", value data.userName, disabled isDisabled
                , onInput ( \newName -> DisplayPartialData { data | userName = newName } )
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
                        DisplayPartialData { data | class = Maybe.map (\newName -> partialClass env.classDescriptions newName) newClass }
                    )
                ]
                <| List.map (\d -> option d.className) env.classDescriptions
            ]
        , div [ class "form-group", class "col-md-6" ]
            [ label [ ] [ text "Rolle" ]
            , niceSelect 
                [ selectedValue ( Maybe.andThen (\cls -> cls.role) data.class )
                , disabled (isDisabled || data.class == Nothing)
                , onUpdate (\newSpec -> DisplayPartialData <| updateClass (\cls -> Just { cls | role = newSpec }) data )
                ]
                ( Maybe.andThen ( \cls -> getRoles cls.className env.classDescriptions ) data.class
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
                        , onInput (Just >> updateFun >> DisplayPartialData)
                        ]
                        [ ]
                Just lootLocations ->
                    niceSelect
                        [ selectedValue itemName, searchable, nullable, disabled isDisabled
                        , onUpdate (updateFun >> DisplayPartialData)
                        ]
                        <| List.map (\l -> optionGroup l.locationName l.loot ) lootLocations
            ]