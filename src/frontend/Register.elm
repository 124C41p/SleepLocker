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
    , User
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

type Model
    = InvalidModel String
    | ValidModel ModelData

type alias ModelData = 
    { state : State
    , env : Environment
    , timeZone : Zone
    }

type State
    = Loading
    | DisplayingStoredData
        { user : User
        , softlocks : Array SoftlockItem
        , infoMessage : InfoMessage
        }
    | EditingForm
        { formData : FormData
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

markdownDecoder : Decoder (Html msg)
markdownDecoder =
    Decode.string
    |> Decode.map
        ( Markdown.toHtml
            ( Just { defaultOptions | rawHtml = MDConfig.DontParse } )
        >> div []
        )

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
        ( Decode.field "comments" <| Decode.nullable markdownDecoder )
        ( Decode.field "title" Decode.string )
        ( Decode.field "createdOn" timeStringDecoder )
        ( Decode.field "numPriorities" Decode.int )
    
getRoles : String -> List ClassDescription -> Maybe (List String)
getRoles className =
    List.filter (\d -> d.className == className)
        >> List.head
        >> Maybe.map (\d -> d.roles)

type alias FormData =
    { userName : String
    , class : Maybe PartialClass
    , softlocks : Array SoftlockItem
    }

type alias PartialClass =
    { className : String
    , role : Maybe String
    }

emptyFormData : Int -> FormData
emptyFormData numPriorities = FormData "" Nothing (Array.initialize numPriorities (always Nothing))

updateClass : (PartialClass -> Maybe PartialClass) -> FormData -> FormData
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

validateItem : SoftlockItem -> SoftlockItem
validateItem = Maybe.andThen (\name -> if String.isEmpty (String.trim name) then Nothing else Just name)

validateFormData : FormData -> Maybe (User, Array SoftlockItem)
validateFormData data =
    Maybe.map3 User
        (let
            trimmedName = String.trim data.userName
            len = String.length trimmedName
        in
            if len == 0 || len > 50 then Nothing else Just trimmedName)
        (Maybe.map (\class -> class.className) data.class)
        (Maybe.andThen (\class -> class.role) data.class)
    |> Maybe.map (\user -> (user, Array.map validateItem data.softlocks))

invalidateUserData : User -> Array SoftlockItem -> FormData
invalidateUserData user locks =
    FormData
        user.userName
        (Just <| PartialClass user.class (Just user.role))
        locks

loadUserData : RaidUserKey -> UserID -> InfoMessage -> Cmd Msg
loadUserData raidID userID infoMsg =
    loadRegistration
        raidID
        userID
        ( ResultX.unwrap
            DisplayEmptyData
            ( \(user, softlocks) ->
                DisplayLockedData infoMsg user softlocks
            )
        )
        ( Just <| WaitAndReload infoMsg )

storeUserData : RaidUserKey -> UserID -> User -> Array SoftlockItem -> Cmd Msg
storeUserData raidID userID user locks =
    storeRegistration raidID userID user locks
        ( \res -> case res of
            Err message -> DisplayEditableData (ErrorMsg message) (invalidateUserData user locks)
            Ok () -> Reload (SuccessMsg "Du bist jetzt angemeldet.")
        )

clearUserData : RaidUserKey -> UserID -> User -> Array SoftlockItem -> Cmd Msg
clearUserData raidID userID user locks =
    deleteRegistration
        raidID
        userID
        ( \result ->
            case result of
                Err errorMsg -> Reload (ErrorMsg errorMsg)
                Ok () -> DisplayEditableData (SuccessMsg "Deine Anmeldung wurde storniert.") (invalidateUserData user locks)
        )

init : Flags -> (Model, Cmd Msg)
init flags =
    case Decode.decodeValue environmentDecoder flags of
        Ok env -> 
            ( ValidModel
                { state = Loading
                , timeZone = Time.utc
                , env = env
                }
            , Cmd.batch
                [ loadUserData env.raidID env.userID NoMsg
                , loadTimeZone NewTimeZone
                ]
            )
        Err err ->
            ( InvalidModel (Decode.errorToString err)
            , Cmd.none
            )

type Msg
    = Reload InfoMessage
    | WaitAndReload InfoMessage
    | DisplayEmptyData
    | DisplayEditableData InfoMessage FormData
    | DisplayLockedData InfoMessage User (Array SoftlockItem)
    | RegisterUserData User (Array SoftlockItem)
    | CancelUserData User (Array SoftlockItem)
    | NewTimeZone Zone

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case model of
        InvalidModel _ -> (model, Cmd.none)
        ValidModel data ->
            let
                (newData, cmd) = updateValidModel msg data
            in
                (ValidModel newData, cmd)

updateValidModel : Msg -> ModelData -> (ModelData, Cmd Msg)
updateValidModel msg model =
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
            ( { model | state = EditingForm { formData = emptyFormData model.env.numPriorities, infoMessage = NoMsg} }
            , Cmd.none
            )
        DisplayLockedData infoMsg user locks ->
            ( { model | state = DisplayingStoredData { user = user, softlocks = locks, infoMessage = infoMsg } }
            , Cmd.none
            )
        RegisterUserData user locks ->
            ( { model | state = Loading }
            , storeUserData model.env.raidID model.env.userID user locks
            )
        CancelUserData user locks ->
            ( { model | state = Loading }
            , clearUserData model.env.raidID model.env.userID user locks
            )
        DisplayEditableData infoMsg formData ->
            ( { model | state = EditingForm { formData = formData, infoMessage = infoMsg } }
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
    case model of
        InvalidModel err -> viewInitError err
        ValidModel data -> viewValidModel data

viewValidModel : ModelData -> Html Msg
viewValidModel data =
    viewFormBorder data.env data.timeZone <|
        case data.state of
            Loading -> viewLoading
            EditingForm { formData, infoMessage } -> viewFormEditable data.env formData infoMessage
            DisplayingStoredData { user, softlocks, infoMessage } -> viewFormLocked data.env user softlocks infoMessage

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

viewFormEditable : Environment -> FormData -> InfoMessage -> Html Msg
viewFormEditable env data infoMsg =
    div []
        [ viewInfoMsg infoMsg
        , viewInputForm env data False
        , case validateFormData data of
            Nothing ->
                div [ class "btn-group" ]
                    [ button [ class "btn", class "btn-secondary", disabled True ] [ text "Abschicken" ]
                    , button [ class "btn", class "btn-secondary", disabled True ] [ text "Stornieren" ]
                    ]
            Just (user, locks) ->
                div [ class "btn-group" ]
                    [ button [ class "btn", class "btn-primary", onClick (RegisterUserData user locks) ] [ text "Abschicken" ]
                    , button [ class "btn", class "btn-secondary", disabled True ] [ text "Stornieren" ]
                    ]
        ]
       
viewFormLocked : Environment -> User -> Array SoftlockItem -> InfoMessage -> Html Msg
viewFormLocked env user locks infoMsg =
    div []
        [ viewInfoMsg infoMsg
        , viewInputForm env (invalidateUserData user locks) True
        , div [ class "btn-group" ]
            [ button [ class "btn", class "btn-secondary", disabled True ] [ text "Abschicken" ]
            , button [ class "btn", class "btn-primary", onClick (CancelUserData user locks) ] [ text "Stornieren" ]
            ]
        ]

viewInfoMsg : InfoMessage -> Html Msg
viewInfoMsg infoMsg =
    case infoMsg of
        NoMsg -> div [] []
        ErrorMsg message ->
            div [ class "alert", class "alert-danger" ]
                [ span [] [ text message ]
                ]
        SuccessMsg message ->
            div [ class "alert", class "alert-success" ]
                [ span [] [ text message ]
                ]

viewInputForm : Environment -> FormData -> Bool -> Html Msg
viewInputForm env data isDisabled =
    let
        userParts =
            [ div [ class "form-group" ]
                [ label [ for "userName" ] [ text "Charaktername" ]
                , input 
                    [ class "form-control", id "userName", value data.userName, disabled isDisabled
                    , onInput ( \newName -> DisplayEditableData NoMsg { data | userName = newName } )
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
                                DisplayEditableData NoMsg { data | class = Maybe.map (\newName -> partialClass env.classDescriptions newName) newClass }
                            )
                        ]
                        <| List.map (\d -> option d.className) env.classDescriptions
                    ]
                , div [ class "form-group", class "col-md-6" ]
                    [ label [ ] [ text "Rolle" ]
                    , niceSelect 
                        [ selectedValue ( Maybe.andThen (\cls -> cls.role) data.class )
                        , disabled (isDisabled || data.class == Nothing)
                        , onUpdate (\newSpec -> DisplayEditableData NoMsg <| updateClass (\cls -> Just { cls | role = newSpec }) data )
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

viewInputFormSoftlock : Maybe (List ItemLocation) -> Bool -> (SoftlockItem -> FormData) -> Int -> SoftlockItem -> Html Msg
viewInputFormSoftlock loot isDisabled updateFun index item  =
    div [ class "form-group" ]
            [ label [ ] [ text ("Prio " ++ String.fromInt (index + 1)) ]
            , case loot of
                Nothing ->
                    input 
                        [ class "form-control", id "userName", value (Maybe.withDefault "" item)
                        , disabled isDisabled
                        , onInput (Just >> updateFun >> DisplayEditableData NoMsg)
                        ]
                        [ ]
                Just lootLocations ->
                    niceSelect
                        [ selectedValue item, searchable, nullable, disabled isDisabled
                        , onUpdate (updateFun >> DisplayEditableData NoMsg)
                        ]
                        <| List.map (\l -> optionGroup l.locationName l.loot ) lootLocations
            ]