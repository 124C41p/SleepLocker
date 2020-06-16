module Register exposing (main)
import Html exposing (Html, div, h4, text, label, input, button, span, hr)
import Html.Attributes exposing (class, for, value, id, attribute, style, disabled)
import Html.Events exposing (onInput, onClick)
import Browser
import NiceSelect exposing (niceSelect, option, optionGroup, selectedValue, searchable, nullable, onUpdate)
import Json.Decode as Decode exposing (Decoder)
import Helpers exposing (delay, loadTimeZone, viewTitle, viewInitError)
import Markdown
import Markdown.Config as MDConfig exposing (defaultOptions)
import Maybe.Extra as MaybeX
import Result.Extra as ResultX
import Time exposing (Posix, Zone)
import Array exposing (Array)
import Api exposing
    ( loadRegistration
    , storeRegistration
    , RaidUserKey
    , UserID
    , ClassDescription
    , classDescriptionDecoder
    , ItemLocation
    , itemLocationDecoder
    , timeStringDecoder
    , raidUserKeyDecoder
    , userIDDecoder
    , deleteRegistration
    , SoftlockItem
    )

main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
    }

type alias Flags = Decode.Value

type alias Model = 
    { state : State
    , env : ParsedEnvironment
    , timeZone : Zone
    }
type State
    = Loading
    | DisplayingStoredData
        { userData : FormData
        , infoMessage : InfoMessage
        }
    | EditingPartialData PartialFormData
    | EditingCompleteData
        { userData : FormData
        , infoMessage : InfoMessage
        }

type InfoMessage
    = NoMsg
    | SuccessMsg String
    | ErrorMsg String

type alias Environment =
    { lootTable : Maybe (List ItemLocation)
    , classDescriptions : List ClassDescription
    , raidID : RaidUserKey
    , userID : UserID
    , comments : Maybe (Html Msg)
    , title : String
    , createdOn : Posix
    , numPriorities : Int
    }

environmentDecoder : Decoder Environment
environmentDecoder =
    Decode.map8 Environment
        ( Decode.field "lootTable" 
            <| Decode.nullable
            <| Decode.list
            <| itemLocationDecoder
        )
        ( Decode.field "classDescriptions"
            <| Decode.list classDescriptionDecoder
        )
        ( Decode.field "raidID" raidUserKeyDecoder )
        ( Decode.field "userID" userIDDecoder )
        ( Decode.field "comments"
            <| Decode.nullable
                ( Decode.string
                    |> Decode.map
                        ( Markdown.toHtml ( Just { defaultOptions | rawHtml = MDConfig.DontParse } )
                            >> div []
                        )
                )
        )
        ( Decode.field "title" Decode.string )
        ( Decode.field "createdOn" timeStringDecoder )
        ( Decode.field "numPriorities" Decode.int )

type ParsedEnvironment
    = InvalidEnvironment String
    | ValidEnvironment Environment
    
getRoles : String -> List ClassDescription -> Maybe (List String)
getRoles className =
    List.filter (\d -> d.className == className)
        >> List.head
        >> Maybe.map (\d -> d.roles)

type alias FormData =
    { userName : String
    , class : String
    , role : String
    , softlocks : Array SoftlockItem
    }

type alias PartialFormData =
    { userName : String
    , class : Maybe PartialClass
    , softlocks : Array SoftlockItem
    }

type alias PartialClass =
    { className : String
    , role : Maybe String
    }

emptyUserData : Int -> PartialFormData
emptyUserData numPriorities = PartialFormData "" Nothing (Array.initialize numPriorities (always Nothing))

updateClass : (PartialClass -> Maybe PartialClass) -> PartialFormData -> PartialFormData
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

validateUserData : PartialFormData -> Maybe FormData
validateUserData data =
    Maybe.map3 FormData
        (let
            len = String.length (String.trim data.userName)
        in
            if len == 0 || len > 50 then Nothing else Just data.userName)
        (Maybe.map (\class -> class.className) data.class)
        (Maybe.andThen (\class -> class.role) data.class)
    |> Maybe.map (\partialData -> partialData data.softlocks)

invalidateUserData : FormData -> PartialFormData
invalidateUserData data =
    PartialFormData
        data.userName
        (Just <| PartialClass data.class (Just data.role))
        data.softlocks

buildState : PartialFormData -> State
buildState data =
    case validateUserData data of
        Nothing -> EditingPartialData data
        Just completeData -> EditingCompleteData { userData = completeData, infoMessage = NoMsg }

loadUserData : RaidUserKey -> UserID -> InfoMessage -> Cmd Msg
loadUserData raidID userID infoMsg =
    loadRegistration
        raidID
        userID
        ( ResultX.unwrap
            DisplayEmptyData
            ( \(user, softlocks) ->
                DisplayLockedData
                    infoMsg
                    { userName = user.userName
                    , class = user.class
                    , role = user.role
                    , softlocks = softlocks
                    }
            )
        )
        ( Just <| WaitAndReload infoMsg )

storeUserData : RaidUserKey -> UserID -> FormData -> Cmd Msg
storeUserData raidID userID data =
    storeRegistration
        raidID
        userID
        { userName = String.trim data.userName
        , class = data.class
        , role = data.role
        }
        data.softlocks
        ( \res -> case res of
            Err message -> DisplayEditableData (ErrorMsg message) data
            Ok () -> Reload (SuccessMsg "Du bist jetzt angemeldet.")
        )

clearUserData : RaidUserKey -> UserID -> FormData -> Cmd Msg
clearUserData raidID userID data =
    deleteRegistration
        raidID
        userID
        ( \result ->
            case result of
                Err errorMsg -> Reload (ErrorMsg errorMsg)
                Ok () -> DisplayEditableData (SuccessMsg "Deine Anmeldung wurde storniert.") data
        )

init : Flags -> (Model, Cmd Msg)
init flags =
    case Decode.decodeValue environmentDecoder flags of
        Ok env -> 
            (
                { state = Loading
                , timeZone = Time.utc
                , env = ValidEnvironment env
                }
            , Cmd.batch
                [ loadUserData env.raidID env.userID NoMsg
                , loadTimeZone NewTimeZone
                ]
            )
        Err err ->
            (
                { state = Loading
                , timeZone = Time.utc
                , env = InvalidEnvironment (Decode.errorToString err)
                }
            , Cmd.none
            )

type Msg
    = Reload InfoMessage
    | WaitAndReload InfoMessage
    | DisplayEmptyData
    | DisplayEditableData InfoMessage FormData
    | DisplayLockedData InfoMessage FormData
    | DisplayPartialData PartialFormData
    | RegisterUserData FormData
    | CancelUserData FormData
    | NewTimeZone Zone

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case model.env of
        InvalidEnvironment _ -> (model, Cmd.none)
        ValidEnvironment env ->
            case msg of
                Reload infoMsg ->
                    ( { model | state = Loading }
                    , loadUserData env.raidID env.userID infoMsg
                    )
                WaitAndReload infoMsg ->
                    ( { model | state = Loading }
                    , delay 1000 (Reload infoMsg)
                    )
                DisplayEmptyData ->
                    ( { model | state = EditingPartialData (emptyUserData env.numPriorities) }
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
                    , storeUserData env.raidID env.userID data
                    )
                CancelUserData data ->
                    ( { model | state = Loading }
                    , clearUserData env.raidID env.userID data
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
view model =
    case model.env of
        InvalidEnvironment err -> viewInitError err
        ValidEnvironment env ->
            viewFormBorder env model.timeZone <|
                case model.state of
                    Loading -> viewLoading
                    EditingPartialData data -> viewFormPartial env data
                    DisplayingStoredData { userData, infoMessage } -> viewFormLocked env userData infoMessage
                    EditingCompleteData { userData, infoMessage } -> viewFormComplete env userData infoMessage

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

viewFormPartial : Environment -> PartialFormData -> Html Msg
viewFormPartial env data =
    div []
        [ viewInputForm env data False
        , div [ class "btn-group" ]
            [ button [ class "btn", class "btn-secondary", disabled True ] [ text "Abschicken" ]
            , button [ class "btn", class "btn-secondary", disabled True ] [ text "Stornieren" ]
            ]
        ]
        
viewFormComplete : Environment -> FormData -> InfoMessage -> Html Msg
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

viewFormLocked : Environment -> FormData -> InfoMessage -> Html Msg
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

viewInputForm : Environment -> PartialFormData -> Bool -> Html Msg
viewInputForm env data isDisabled =
    let
        userParts =
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
            ]
        softlockParts = 
            List.indexedMap 
                ( \index item ->
                    viewInputFormSoftlock 
                        env.lootTable
                        isDisabled
                        (\newItem ->
                            { data | softlocks = Array.set index newItem data.softlocks }
                        )
                        index
                        item
                )
                (Array.toList data.softlocks)
    in
        div [] (userParts ++ softlockParts)

viewInputFormSoftlock : Maybe (List ItemLocation) -> Bool -> (SoftlockItem -> PartialFormData) -> Int -> SoftlockItem -> Html Msg
viewInputFormSoftlock loot isDisabled updateFun index item  =
    div [ class "form-group" ]
            [ label [ ] [ text ("Prio " ++ String.fromInt (index + 1)) ]
            , case loot of
                Nothing ->
                    input 
                        [ class "form-control", id "userName", value (Maybe.withDefault "" item)
                        , disabled isDisabled
                        , onInput (Just >> updateFun >> DisplayPartialData)
                        ]
                        [ ]
                Just lootLocations ->
                    niceSelect
                        [ selectedValue item, searchable, nullable, disabled isDisabled
                        , onUpdate (updateFun >> DisplayPartialData)
                        ]
                        <| List.map (\l -> optionGroup l.locationName l.loot ) lootLocations
            ]