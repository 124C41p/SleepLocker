module Tables exposing (main)

import Html exposing (Html, div, text, table, thead, tbody, th, tr, td, ul, li, a, h4, del)
import Html.Attributes exposing (class, scope, href, classList)
import Html.Events exposing (onClick)
import Browser
import Time exposing (Posix, Zone)
import Helpers exposing (loadTimeZone, viewTitle, viewInitError)
import Platform.Cmd exposing (Cmd)
import Api exposing (User, SoftlockItem, LootTable, LootTableItem, userDecoder, softlockItemDecoder, timeStringDecoder, lootTableDecoder)
import Json.Decode as Decode exposing (Decoder)
import Array exposing (Array)
import Maybe.Extra as MaybeX

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
    { userList : List (User, Array SoftlockItem)
    , title : String
    , createdOn : Posix
    , numPriorities : Int
    , locationSoftlocks : List LocationSoftlocks
    , timeZone : Zone
    , state : State
    }

flagsDecoder : Decoder ModelData
flagsDecoder =
    Decode.map5
        ( \users title time numPrios table ->
            ModelData users title time numPrios (buildLocationList numPrios users table) Time.utc CompleteList
        )
        ( Decode.field "userList"
            <| Decode.list
            <| Decode.map2 (\user locks -> (user,locks))
                userDecoder
                (Decode.field "softlocks" (Decode.array softlockItemDecoder))
        )
        ( Decode.field "title" Decode.string )
        ( Decode.field "createdOn" timeStringDecoder )
        ( Decode.field "numPriorities" Decode.int )
        ( Decode.field "lootInformation" lootTableDecoder )

type State
    = CompleteList
    | LocationLists

type alias LocationSoftlocks =
    { locationName : String
    , softlocks : List SoftlockedItem
    }

type alias SoftlockedItem =
    { itemName : String
    , userLocks : Array (List String)
    }

buildLocationList : Int -> List (User, Array SoftlockItem) -> LootTable -> List LocationSoftlocks
buildLocationList numPriorities userList env =
    List.map
        ( \locationName ->
            { locationName = locationName
            , softlocks =
                buildSoftlockList numPriorities userList (locationLoot locationName env.loot)
                |> List.filter
                    (\item ->
                        item.userLocks
                        |> Array.toList
                        |> List.any (not << List.isEmpty)
                    )
            }
        )
        env.locations

locationLoot : String -> List LootTableItem -> List String
locationLoot locationName itemList =
    List.filter (\item -> List.member locationName item.locations) itemList
    |> List.map (\item -> item.itemName)

buildSoftlockList : Int -> List (User, Array SoftlockItem) -> List String -> List SoftlockedItem
buildSoftlockList numPriorities userList itemList =
    List.map
        ( \itemName ->
            { itemName = itemName
            , userLocks =
                Array.initialize numPriorities identity
                |> Array.map 
                    (\i ->
                        userList
                        |> List.filter
                            (\(_,locks) -> 
                                Array.get i locks
                                |> Maybe.andThen identity
                                |> MaybeX.unwrap False ((==) itemName)
                            ) 
                        |> List.map (\(user,_) -> user.userName)
                    )
            }
        )
        itemList

init : Flags -> (Model, Cmd Msg)
init flags =
    case Decode.decodeValue flagsDecoder flags of
        Ok data ->
            ( ValidModel data
            , loadTimeZone NewTimeZone
            )
        Err err -> 
            ( InvalidModel (Decode.errorToString err)
            , Cmd.none
            )

type Msg
    = ChangeState State
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
view model =
    case model of
        InvalidModel err -> viewInitError err
        ValidModel data -> viewValidModel data

viewValidModel : ModelData -> Html Msg
viewValidModel data =
    div [ class "container-fluid", class "py-5" ]
        [ viewTitle data.timeZone  data.title data.createdOn
        ,
            if List.isEmpty data.locationSoftlocks then
                div [] []
            else
                viewTabBar data.state
        ,
            case data.state of
                CompleteList -> viewUserList data.numPriorities data.userList
                LocationLists -> viewLocationLists data.numPriorities data.locationSoftlocks
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

viewUserList : Int -> List (User, Array SoftlockItem) -> Html Msg
viewUserList numPriorities userList =
    let
        headStatics =
            [ th [ scope "col" ] [ text "#" ]
                , th [ scope "col" ] [ text "Name" ]
                , th [ scope "col" ] [ text "Klasse" ]
                , th [ scope "col" ] [ text "Rolle" ]
            ]
        headSoftlocks =
            List.range 1 numPriorities
            |> List.map
                ( String.fromInt
                >> (++) "Prio "
                >> text
                >> List.singleton
                >> th [ scope "col" ]
                )
    in
        div [ class "row", class "d-flex", class "justify-content-center", class "mt-5" ]
        [ div [ class "col" ]
            [ table [ class "table", class "table-striped", class "table-bordered" ]
                [ thead [] ( headStatics ++  headSoftlocks)
                , tbody [] ( List.indexedMap viewUserRow userList )
                ]
            ]
        ]

viewUserRow : Int -> (User, Array SoftlockItem) -> Html Msg
viewUserRow index (user, locks) =
    let
        staticPart = 
            [ th [ scope "row" ] [ text ( String.fromInt (index + 1) ) ]
            , td [] [ text user.userName ]
            , td [] [ text user.class ]
            , td [] [ text user.role ]
            ]
        locksPart =
            locks
            |> Array.toList
            |> List.map
                ( Maybe.withDefault ""
                >> text
                >> List.singleton
                >> td []
                )
    in
        tr [] ( staticPart ++ locksPart )

viewLocationLists : Int -> List LocationSoftlocks -> Html Msg
viewLocationLists numPriorities locationList =
    div [] (List.map (viewLocationList numPriorities) locationList)

viewLocationList : Int -> LocationSoftlocks -> Html Msg
viewLocationList numPriorities location =
    div [ class "row", class "d-flex", class "justify-content-center", class "mt-5" ]
    [ div [ class "col" ]
        (if List.isEmpty location.softlocks then
            [ h4 [ class "text-center" ] 
                [ del [] [ text location.locationName ]
                ]
            ]
         else
            let
                headStatics =
                    [ th [ scope "col" ] [ text "Item" ]
                    ]
                headSoftlocks =
                    List.range 1 numPriorities
                    |> List.map
                        ( String.fromInt
                        >> (++) "Prio "
                        >> text
                        >> List.singleton
                        >> th [ scope "col" ]
                        )
            in
                [ h4 [ class "text-center" ] [ text location.locationName ]
                , div [ class "card-body" ]
                    [ table [ class "table", class "table-striped", class "table-bordered" ]
                        [ thead [] ( headStatics ++ headSoftlocks )
                        , tbody [] ( List.map viewSoftlockRow location.softlocks )
                        ]
                    ]
                ]
        )
    ]

viewSoftlockRow : SoftlockedItem -> Html Msg
viewSoftlockRow item =
    tr []
        ( td [ ] [ text item.itemName ] ::
            ( item.userLocks
                |> Array.toList
                |> List.map
                    ( String.join ", "
                    >> text
                    >> List.singleton
                    >> td []
                    )
            )
        )