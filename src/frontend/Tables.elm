module Tables exposing (main)

import Html exposing (Html, div, text, table, thead, tbody, th, tr, td, ul, li, a, h4, del)
import Html.Attributes exposing (class, scope, href, classList)
import Html.Events exposing (onClick)
import Browser
import UserData exposing (UserData)
import Time exposing (Posix, Zone, millisToPosix)
import Iso8601 exposing (toTime)
import String exposing (String)
import Helpers exposing (loadTimeZone, viewTitle)
import Platform.Cmd exposing (Cmd)


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
    }

type alias Flags =
    { userList : List UserData
    , lootInformation: Maybe Environment
    , title : String
    , createdOn : String
    }

type alias Model =
    { state : State
    , userList : List UserData
    , locationSoftlocks : List LocationSoftlocks
    , timeZone : Zone
    , title : String
    , createdOn : Posix
    }

type State
    = CompleteList
    | LocationLists

type alias Environment =
    { locations : List String
    , loot : List Item
    }

type alias Item =
    { itemName : String
    , locations : List String
    }

type alias LocationSoftlocks =
    { locationName : String
    , softlocks : List Softlock
    }

type alias Softlock =
    { itemName : String
    , prio1 : List String
    , prio2 : List String
    }

buildLocationList : List UserData -> Environment -> List LocationSoftlocks
buildLocationList userList env =
    List.map
        ( \locationName ->
            { locationName = locationName
            , softlocks =
                buildSoftlockList userList (locationLoot locationName env.loot)
                |> List.filter (\l -> not <| List.isEmpty l.prio1 && List.isEmpty l.prio2)
            }
        )
        env.locations

locationLoot : String -> List Item -> List String
locationLoot locationName itemList =
    List.filter (\item -> List.member locationName item.locations) itemList
    |> List.map (\item -> item.itemName)

buildSoftlockList : List UserData -> List String -> List Softlock
buildSoftlockList userList itemList =
    List.map
        ( \itemName ->
            { itemName = itemName
            , prio1 =
                List.filter (\user -> user.prio1 == Just itemName) userList
                |> List.map (\user -> user.userName)
            , prio2 =
                List.filter (\user -> user.prio2 == Just itemName) userList
                |> List.map (\user -> user.userName)
            }
        )
        itemList

init : Flags -> (Model, Cmd Msg)
init { userList, lootInformation, title, createdOn } =
    (
        { state = CompleteList
        , userList = userList
        , locationSoftlocks = Maybe.withDefault [] <| Maybe.map (buildLocationList userList) lootInformation
        , timeZone = Time.utc
        , title = title
        , createdOn =
            case toTime createdOn of
                Ok time -> time
                Err _ -> millisToPosix 0
        }
    , loadTimeZone NewTimeZone
    )

type Msg
    = ChangeState State
    | NewTimeZone Zone

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ChangeState newState ->
            ( { model | state = newState }
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
view { state, userList, locationSoftlocks, title, createdOn, timeZone } =
    div [ class "container-fluid", class "py-5" ]
        [ viewTitle timeZone  title createdOn
        ,
            if List.isEmpty locationSoftlocks then
                div [] []
            else
                viewTabBar state
        ,
            case state of
                CompleteList -> viewUserList userList
                LocationLists -> viewLocationLists locationSoftlocks
        ]

viewTabBar : State -> Html Msg
viewTabBar state =
    ul [ class "nav", class "nav-tabs", class "nav-fill" ]
        [ li [class "nav-item", onClick (ChangeState CompleteList)]
            [ a
                [ class "nav-link", href "#"
                , classList [ ("active", state == CompleteList) ]
                ]
                [ text "Gesamtliste" ]
            ]
        , li [class "nav-item", onClick (ChangeState LocationLists)]
            [ a
                [ class "nav-link"
                , href "#" 
                , classList [ ("active", state == LocationLists) ]
                ]
                [ text "Bosslisten" ]
            ]
        ]

viewUserList : List UserData -> Html Msg
viewUserList userList =
    div [ class "row", class "d-flex", class "justify-content-center", class "mt-5" ]
    [ div [ class "col" ]
        [ table [ class "table", class "table-striped", class "table-bordered" ]
            [ thead []
                [ th [ scope "col" ] [ text "#" ]
                , th [ scope "col" ] [ text "Name" ]
                , th [ scope "col" ] [ text "Klasse" ]
                , th [ scope "col" ] [ text "Rolle" ]
                , th [ scope "col" ] [ text "Prio 1" ]
                , th [ scope "col" ] [ text "Prio 2" ]
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
        , td [] [ text ( Maybe.withDefault "" userData.prio1 ) ]
        , td [] [ text ( Maybe.withDefault "" userData.prio2 ) ]
        ]

viewLocationLists : List LocationSoftlocks -> Html Msg
viewLocationLists locationList =
    div [] (List.map viewLocationList locationList)

viewLocationList : LocationSoftlocks -> Html Msg
viewLocationList location =
    div [ class "row", class "d-flex", class "justify-content-center", class "mt-5" ]
    [ div [ class "col" ]
        (if List.isEmpty location.softlocks then
            [ h4 [ class "text-center" ] 
                [ del [] [ text location.locationName ]
                ]
            ]
         else
            [ h4 [ class "text-center" ] [ text location.locationName ]
            , div [ class "card-body" ]
                [ table [ class "table", class "table-striped", class "table-bordered" ]
                    [ thead []
                        [ th [ scope "col" ] [ text "Item" ]
                        , th [ scope "col" ] [ text "Prio 1" ]
                        , th [ scope "col" ] [ text "Prio 2" ]
                        ]
                    , tbody [] ( List.map viewSoftlockRow location.softlocks )
                    ]
                ]
            ]
        )
    ]

viewSoftlockRow : Softlock -> Html Msg
viewSoftlockRow { itemName, prio1, prio2 } =
    tr []
        [ td [ ] [ text itemName ]
        , td [ ] [ text <| String.join ", " prio1 ]
        , td [ ] [ text <| String.join ", " prio2 ]
        ]