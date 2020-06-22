module Admin exposing (main)

import Browser
import Helpers exposing (loadTimeZone, viewTitle, dateString, timeString, viewInitError, viewQuestionModal)
import Html exposing (Html, button, div, input, label, span, table, tbody, td, text, th, thead, tr, p, b)
import Html.Attributes exposing (attribute, class, classList, disabled, scope, value)
import Html.Events exposing (onClick)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Maybe.Extra as MaybeX
import Result.Extra as ResultX
import Time exposing (Posix, Zone)
import Api exposing
    ( RaidAdminKey
    , RaidUserKey
    , User
    , RaidMode(..)
    , RaidStatus
    , getRaidStatus
    , setRaidMode
    , showRaidUserKey
    , raidAdminKeyDecoder
    , raidUserKeyDecoder
    , timeStringDecoder
    , adminRemoveUser
    )

main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

type Model
    = InvalidModel String
    | ValidModel ModelData

type alias ModelData =
    { raidAdminKey : RaidAdminKey
    , raidUserKey : RaidUserKey
    , title : String
    , createdOn : Posix
    , userList : List UserData
    , errorMessage : Maybe String
    , raidMode : Maybe RaidMode
    , timeZone : Zone
    , askDelete : Maybe String
    , sorting : Sorting
    }

type alias UserData =
    { user : User
    , registeredOn : Posix
    , index : Int
    }

type Sorting
    = DateASC
    | DateDSC
    | NameASC
    | NameDSC
    | ClassASC
    | ClassDSC
    | RoleASC
    | RoleDSC

ascSuffix : String
ascSuffix = " ⏶"

dscSuffix : String
dscSuffix = " ⏷"

sortUsers : Sorting -> List UserData -> List UserData
sortUsers sorting userList =
    let
        flipped cmp =
            case cmp of
            LT -> GT
            EQ -> EQ
            GT -> LT
        compareStr str1 str2 =
            compare (String.toLower str1) (String.toLower str2)
        secondComparator cmp user1 user2 =
            case cmp user1 user2 of
                EQ -> compareStr user1.user.userName user2.user.userName
                other -> other
        basicComparator user1 user2 =
            case sorting of
                DateASC -> compare user1.index user2.index
                DateDSC -> flipped <| compare user1.index user2.index
                NameASC -> compareStr user1.user.userName user2.user.userName
                NameDSC -> flipped <| compareStr user1.user.userName user2.user.userName
                ClassASC -> compare user1.user.class user2.user.class
                ClassDSC -> flipped <| compare user1.user.class user2.user.class
                RoleASC -> compare user1.user.role user2.user.role
                RoleDSC -> flipped <| compare user1.user.role user2.user.role
    in
        List.sortWith (secondComparator basicComparator) userList

flagsDecoder : Decoder ModelData
flagsDecoder = 
    Decode.map4 ModelData
        (Decode.field "raidAdminKey" raidAdminKeyDecoder)
        (Decode.field "raidUserKey" raidUserKeyDecoder)
        (Decode.field "title" Decode.string)
        (Decode.field "createdOn" timeStringDecoder)
    |> Decode.map (\p -> p [] Nothing Nothing Time.utc Nothing DateASC)

type alias Flags = Encode.Value

init : Flags -> ( Model, Cmd Msg )
init flags =
    case Decode.decodeValue flagsDecoder flags of
        Ok data -> 
            ( ValidModel data
            , Cmd.batch
                [ loadRegistrations data.raidAdminKey
                , loadTimeZone NewTimeZone
                ]
            )
        Err err ->
            ( InvalidModel (Decode.errorToString err)
            , Cmd.none
            )

type Msg
    = DoUpdate
    | Updated RaidStatus
    | DoSetMode RaidMode
    | ConnectionLost
    | ModeSet RaidMode
    | NewTimeZone Zone
    | AskDelete String
    | DoDelete String
    | DontDelete
    | SetSorting Sorting

loadRegistrations : RaidAdminKey -> Cmd Msg
loadRegistrations raidAdminKey =
    getRaidStatus raidAdminKey (ResultX.unwrap ConnectionLost Updated)

setMode : RaidAdminKey -> RaidMode -> Cmd Msg
setMode raidAdminKey mode =
    setRaidMode raidAdminKey mode
    <| ResultX.unwrap ConnectionLost
    <| always (ModeSet mode)

delete : RaidAdminKey -> String -> Cmd Msg
delete key userName = adminRemoveUser key userName (ResultX.unwrap ConnectionLost (always DoUpdate))

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        InvalidModel _ -> (model, Cmd.none)
        ValidModel data ->
            let
                (newData, newCmd) = updateValidModel msg data
            in
                (ValidModel newData, newCmd)

updateValidModel : Msg -> ModelData -> ( ModelData, Cmd Msg )
updateValidModel msg model =
    case msg of
        ConnectionLost ->
            ( { model | raidMode = Nothing, errorMessage = Just "Verbindung zum Server verloren..." }, Cmd.none )
        DoUpdate ->
            ( model, loadRegistrations model.raidAdminKey )

        Updated raidStatus ->
            (   { model 
                | userList =
                    raidStatus.registrations
                    |> List.indexedMap
                        (\index (user, time) ->
                            UserData user time index
                        )
                    |> sortUsers model.sorting
                , raidMode = Just raidStatus.mode
                , errorMessage = Nothing
                }
            , Cmd.none
            )

        DoSetMode mode ->
            ( { model | errorMessage = Nothing, raidMode = Nothing }, setMode model.raidAdminKey mode )

        ModeSet mode ->
            ( { model | raidMode = Just mode, errorMessage = Nothing }, Cmd.none )
        
        NewTimeZone zone ->
            ( { model | timeZone = zone }, Cmd.none )
        AskDelete userName ->
            ( { model | askDelete = Just userName }, Cmd.none )
        DoDelete userName ->
            ( { model | askDelete = Nothing }, delete model.raidAdminKey userName )
        DontDelete ->
            ( { model | askDelete = Nothing }, Cmd.none )
        SetSorting sorting ->
            (   { model
                | sorting = sorting
                , userList = sortUsers sorting model.userList
                }
            , Cmd.none
            )

subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 5000 (always DoUpdate)


view : Model -> Html Msg
view model =
    case model of
       InvalidModel err -> viewInitError err
       ValidModel data -> viewValidModel data

viewValidModel : ModelData -> Html Msg
viewValidModel model =
    let
        staticPart =
            [ div [ class "row" ]
                [ div [ class "col" ]
                    [ viewTitle model.timeZone model.title model.createdOn
                    ]
                ]
            , div [ class "row" ]
                [ div [ class "col-md-2" ] [ viewControls model.raidUserKey model.errorMessage model.raidMode ]
                , div [ class "col-md-10" ] [ viewUserList model.timeZone model.sorting model.userList ]
                ]
            , case model.askDelete of
                Nothing -> div [] []
                Just userName -> viewDeletionModal userName
            ]
        modalPart =
            case model.askDelete of
                Nothing -> []
                Just userName ->
                    [ viewDeletionModal userName
                    ]
    in
        div [ class "container-fluid", class "py-5" ]
            ( staticPart ++ modalPart )
        

viewDeletionModal : String -> Html Msg
viewDeletionModal userName =
    viewQuestionModal "Löschen"
        [ p []
            [ text "Soll "
            , b [] [ text userName ]
            , text " wirklich aus der Anmeldeliste gelöscht werden?"
            ]
        ]
        (DoDelete userName) DontDelete

viewControls : RaidUserKey -> Maybe String -> Maybe RaidMode -> Html Msg
viewControls raidUserKey errorMsg mode =
    div [ class "card" ]
        [ div [ class "card-body" ]
            [ MaybeX.unwrap (div [] []) viewErrorMsg errorMsg
            , div [ class "form-group" ]
                [ label [] [ text "Raid ID" ]
                , input [ class "form-control", disabled True, value (showRaidUserKey raidUserKey) ] []
                ]
            , case mode of
                Nothing ->
                    viewSpinner

                Just m ->
                    viewButtons m
            ]
        ]

viewButtons : RaidMode -> Html Msg
viewButtons mode =
    div [ class "list-group" ]
        [ button
            [ class "list-group-item"
            , class "list-group-item-action"
            , classList [ ( "active", mode == RegistrationMode ) ]
            , onClick (DoSetMode RegistrationMode)
            ]
            [ text "1: Anmeldephase" ]
        , button
            [ class "list-group-item"
            , class "list-group-item-action"
            , classList [ ( "active", mode == TablesMode ) ]
            , onClick (DoSetMode TablesMode)
            ]
            [ text "2: Veröffentlichungsphase" ]
        ]

dateSortingSuffix : Sorting -> String
dateSortingSuffix sorting =
    case sorting of
        DateASC -> ascSuffix
        DateDSC -> dscSuffix
        _ -> ""

nameSortingSuffix : Sorting -> String
nameSortingSuffix sorting =
    case sorting of
        NameASC -> ascSuffix
        NameDSC -> dscSuffix
        _ -> ""

classSortingSuffix : Sorting -> String
classSortingSuffix sorting =
    case sorting of
        ClassASC -> ascSuffix
        ClassDSC -> dscSuffix
        _ -> ""

roleSortingSuffix : Sorting -> String
roleSortingSuffix sorting =
    case sorting of
        RoleASC -> ascSuffix
        RoleDSC -> dscSuffix
        _ -> ""

viewUserList : Zone -> Sorting -> List UserData -> Html Msg
viewUserList zone sorting userList =
    table [ class "table", class "table-striped", class "table-bordered" ]
        [ thead []
            [ th [ scope "col", class "align-middle" ]
                [ button [ class "btn", class "btn-link", onClick (SetSorting <| if sorting == DateASC then DateDSC else DateASC) ]
                    [ text <| "#" ++ dateSortingSuffix sorting ]
                ]
            , th [ scope "col", class "align-middle" ]
                [ button [ class "btn", class "btn-link", onClick (SetSorting <| if sorting == NameASC then NameDSC else NameASC) ]
                    [ text <| "Name" ++ nameSortingSuffix sorting ]
                ]
            , th [ scope "col", class "align-middle" ]
                [ button [ class "btn", class "btn-link", onClick (SetSorting <| if sorting == ClassASC then ClassDSC else ClassASC) ]
                    [ text <| "Klasse" ++ classSortingSuffix sorting ]
                ]
            , th [ scope "col", class "align-middle" ]
                [ button [ class "btn", class "btn-link", onClick (SetSorting <| if sorting == RoleASC then RoleDSC else RoleASC) ]
                    [ text <| "Rolle" ++ roleSortingSuffix sorting ]
                ]
            , th [ scope "col", class "align-middle" ]
                [ button [ class "btn", class "btn-link", onClick (SetSorting <| if sorting == DateASC then DateDSC else DateASC) ]
                    [ text <| "Anmeldezeitpunkt" ++ dateSortingSuffix sorting ]
                ]
            , th [ scope "col", class "align-middle" ] [ text "Löschen" ]
            ]
        , tbody [] (List.map (\data -> viewUserRow zone data) userList)
        ]

viewUserRow : Zone -> UserData -> Html Msg
viewUserRow zone data =
    tr []
        [ th [ scope "row" ] [ text (String.fromInt (data.index + 1)) ]
        , td [] [ text data.user.userName ]
        , td [] [ text data.user.class ]
        , td [] [ text data.user.role ]
        , td [] [ text (registeredOnString zone data.registeredOn) ]
        , td []
            [ button [ class "btn", onClick (AskDelete data.user.userName) ]
                [ span [] [ text "❌" ]
                ]
            ]
        ]

registeredOnString : Zone -> Posix -> String
registeredOnString zone time = dateString zone time ++ " um " ++ timeString zone time

viewErrorMsg : String -> Html Msg
viewErrorMsg errorMsg =
    div [ class "alert", class "alert-danger" ]
        [ span [] [ text errorMsg ]
        ]

viewSpinner : Html Msg
viewSpinner =
    div [ class "d-flex", class "justify-content-center" ]
        [ div [ class "spinner-border", class "text-primary", attribute "role" "status" ]
            [ span [ class "sr-only" ] [ text "Lade..." ] ]
        ]
