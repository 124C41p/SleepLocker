module Api exposing
    ( User
    , Raid
    , RaidAdminKey
    , raidAdminKeyDecoder
    , RaidUserKey
    , raidUserKeyDecoder
    , showRaidUserKey
    , UserID
    , userIDDecoder
    , userDecoder
    , RaidMode(..)
    , getRaidInfo
    , setRaidMode
    , loadRegistration
    , storeRegistration
    , deleteRegistration
    , RaidStatus
    , getRaidStatus
    , createRaid
    , ItemLocation
    , itemLocationDecoder
    , ClassDescription
    , classDescriptionDecoder
    , timeStringDecoder
    , SoftlockItem
    , softlockItemDecoder
    , navigateAdminPage
    , navigateUserPage
    , validateRaidUserKey
    , LootTable
    , LootTableItem
    , lootTableDecoder
    , adminRemoveUser
    )

import Json.Decode as Decode exposing(Decoder)
import Json.Encode as Encode
import Http
import Time exposing (Posix)
import Maybe.Extra as MaybeX
import Iso8601 exposing (toTime)
import Array exposing (Array)
import Browser.Navigation as Navigation

responseDecoder : Decoder a -> Decoder (Result String a)
responseDecoder decoder =
    Decode.field "success" Decode.bool
    |> Decode.andThen
        ( \success -> 
            if success then
                Decode.field "result" decoder
                |> Decode.map Ok
            else
                Decode.field "errorMsg" Decode.string
                |> Decode.map Err
        )
    
type alias ProcessFun a msg = Result String a -> msg

expectResponse : ProcessFun a msg -> Maybe msg -> Decoder a -> Http.Expect msg
expectResponse processFun defaultMsg decoder =
    Http.expectJson
        ( Result.map processFun >> Result.withDefault (Maybe.withDefault (processFun <| Err "Unbekannter Fehler. Versuche es später nochmal.") defaultMsg) )
        ( responseDecoder decoder )

type RaidUserKey = RaidUserKey String

raidUserKeyDecoder : Decoder RaidUserKey
raidUserKeyDecoder = Decode.string
    |> Decode.andThen
        ( \str ->
             if String.length str == 6 then
                Decode.succeed (RaidUserKey str)
             else
                Decode.fail "Ungültiger Raidschlüssel."
        )

type RaidAdminKey = RaidAdminKey String

raidAdminKeyDecoder : Decoder RaidAdminKey
raidAdminKeyDecoder = Decode.string
    |> Decode.andThen
        ( \str ->
             if String.length str == 20 then
                Decode.succeed (RaidAdminKey str)
             else
                Decode.fail "Ungültiger Raidschlüssel."
        )

type UserID = UserID String

userIDDecoder : Decoder UserID
userIDDecoder = Decode.string
    |> Decode.andThen
        ( \str ->
             if String.length str == 50 then
                Decode.succeed (UserID str)
             else
                Decode.fail "Ungültige User ID."
        )

type RaidMode
    = RegistrationMode
    | TablesMode

raidModeEncoder : RaidMode -> Encode.Value
raidModeEncoder mode =
    case mode of
        RegistrationMode -> Encode.int 0
        TablesMode -> Encode.int 1

raidModeDecoder : Decoder RaidMode
raidModeDecoder =
    Decode.int
    |> Decode.andThen
        ( \m -> case m of
            0 -> Decode.succeed RegistrationMode
            1 -> Decode.succeed TablesMode
            _ -> Decode.fail "Ungültiger Raid-Modus."
        )

showRaidUserKey : RaidUserKey -> String
showRaidUserKey (RaidUserKey key) = key

type alias User =
    { userName : String
    , class : String
    , role : String
    }

type alias SoftlockItem = Maybe String

softlockItemEncoder : SoftlockItem -> Encode.Value
softlockItemEncoder item =
    Encode.object
        [ ( "itemName", MaybeX.unwrap Encode.null Encode.string item )
        ]
        
softlockItemDecoder : Decoder SoftlockItem
softlockItemDecoder =
    Decode.field "itemName" (Decode.nullable Decode.string)

userDecoder : Decoder User
userDecoder = Decode.map3 User
    (Decode.field "userName" Decode.string)
    (Decode.field "class" Decode.string)
    (Decode.field "role" Decode.string)

registrationDecoder : Decoder (User, Array SoftlockItem)
registrationDecoder =
    Decode.map2 (\user locks -> (user, locks))
        userDecoder
        (Decode.field "softlocks" (Decode.array softlockItemDecoder))

loadRegistration : RaidUserKey -> UserID -> ProcessFun (User, Array SoftlockItem) msg -> Maybe msg -> Cmd msg
loadRegistration (RaidUserKey key) (UserID id) processFun defaultMsg =
    Http.post
        { url = "/api/myData"
        , body = Http.jsonBody
            <| Encode.object
                [ ( "raidUserKey", Encode.string key )
                , ( "userID", Encode.string id )
                ]
        , expect = expectResponse
            processFun
            defaultMsg
            registrationDecoder
        }

storeRegistration : RaidUserKey -> UserID -> User -> Array SoftlockItem -> ProcessFun () msg -> Cmd msg
storeRegistration (RaidUserKey key) (UserID id) user items processFun =
    Http.post
        { url = "/api/register"
        , body = Http.jsonBody
            <| Encode.object
                [ ( "userName", Encode.string user.userName )
                , ( "userID", Encode.string id )
                , ( "raidUserKey", Encode.string key )
                , ( "class", Encode.string user.class )
                , ( "role", Encode.string user.role )
                , ( "softlocks", Encode.array softlockItemEncoder items )
                ]
        , expect =
            expectResponse
            processFun
            Nothing
            ( Decode.null () )
        }
        
deleteRegistration : RaidUserKey -> UserID -> ProcessFun () msg -> Cmd msg
deleteRegistration (RaidUserKey key) (UserID id) processFun =
    Http.post
        { url = "/api/clearMyData"
        , body = Http.jsonBody
            <| Encode.object
                [ ( "raidUserKey", Encode.string key )
                , ( "userID", Encode.string id )
                ]
        , expect =
            expectResponse
            processFun
            Nothing
            ( Decode.null () )
        }

type alias Raid =
    { title : String
    , dungeonKey : Maybe String
    , numPriorities : Int
    , comments : Maybe String
    }
    
raidDecoder : Decoder Raid
raidDecoder =
    Decode.map4 Raid
        (Decode.field "title" Decode.string)
        (Decode.field "dungeonKey" (Decode.nullable Decode.string))
        (Decode.field "numPriorities" Decode.int)
        (Decode.field "comments" (Decode.nullable Decode.string))

raidEncoder : Raid -> Encode.Value
raidEncoder raid = Encode.object
    [ ("title", Encode.string raid.title)
    , ("dungeonKey", MaybeX.unwrap Encode.null Encode.string raid.dungeonKey)
    , ("numPriorities", Encode.int raid.numPriorities)
    , ("comments", MaybeX.unwrap Encode.null Encode.string raid.comments)
    ]

timeStringDecoder : Decoder Posix
timeStringDecoder =
    Decode.string
    |> Decode.andThen
        ( \str ->
            case toTime str of
                Ok time -> Decode.succeed time
                Err _ -> Decode.fail "Invalid time string format."
        )

validateRaidUserKey : String -> ProcessFun RaidUserKey msg -> Cmd msg
validateRaidUserKey keyStr processFun =
    let
        userKey = RaidUserKey keyStr
    in
        getRaidInfo userKey (Result.map (\_ -> userKey) >> processFun)

getRaidInfo : RaidUserKey -> ProcessFun (Raid, RaidMode, Posix) msg -> Cmd msg
getRaidInfo (RaidUserKey key) processFun =
    let
        decoder = Decode.map3 (\raid mode time -> (raid, mode, time))
            raidDecoder
            (Decode.field "mode" raidModeDecoder)
            (Decode.field "createdOn" timeStringDecoder)
    in
        Http.post
            { url = "/api/getRaid"
            , body = Http.jsonBody
                <| Encode.object
                    [ ( "raidUserKey", Encode.string key )
                    ]
            , expect = expectResponse
                processFun
                Nothing
                decoder
            }

setRaidMode : RaidAdminKey -> RaidMode -> ProcessFun () msg -> Cmd msg
setRaidMode (RaidAdminKey key) mode processFun =
    Http.post
        { url = "/api/setMode"
        , body = Http.jsonBody
            <| Encode.object
                [ ( "raidAdminKey", Encode.string key )
                , ( "mode", raidModeEncoder mode )
                ]
        , expect = expectResponse
            processFun
            Nothing
            ( Decode.null () )
        }

type alias RaidStatus =
    { mode : RaidMode
    , registrations : List (User, Posix)
    }

raidStatusDecoder : Decoder RaidStatus
raidStatusDecoder = 
    let
        decoder = Decode.map2 (\user time -> (user, time))
            userDecoder
            (Decode.field "registeredOn" timeStringDecoder)
    in
        Decode.map2 RaidStatus
            (Decode.field "raidMode" raidModeDecoder)
            (Decode.field "registrations" (Decode.list decoder))

getRaidStatus : RaidAdminKey -> ProcessFun RaidStatus msg -> Cmd msg
getRaidStatus (RaidAdminKey key) processFun =
    Http.post
        { url = "/api/getRaidStatus"
        , body = Http.jsonBody
            <| Encode.object
                [ ( "raidAdminKey", Encode.string key )
                ]
        , expect = expectResponse
            processFun
            Nothing
            raidStatusDecoder
        }

createRaid : Raid -> ProcessFun RaidAdminKey msg -> Cmd msg
createRaid raid processFun =
    Http.post
        { url = "/api/createRaid"
        , body = Http.jsonBody (raidEncoder raid)
        , expect = expectResponse
            processFun
            Nothing
            raidAdminKeyDecoder
        }

type alias ItemLocation =
    { locationName : String
    , loot : List String        
    }

itemLocationDecoder : Decoder ItemLocation
itemLocationDecoder =
    Decode.map2 ItemLocation
        (Decode.field "locationName" Decode.string)
        (Decode.field "loot" (Decode.list Decode.string))

type alias ClassDescription =
    { className : String
    , roles : List String
    }

classDescriptionDecoder : Decoder ClassDescription
classDescriptionDecoder =
    Decode.map2 ClassDescription
        (Decode.field "className" Decode.string)
        (Decode.field "roles" (Decode.list Decode.string))
        
navigateAdminPage : RaidAdminKey -> Cmd msg
navigateAdminPage (RaidAdminKey key) = Navigation.load("/" ++ key)

navigateUserPage : RaidUserKey -> Cmd msg
navigateUserPage (RaidUserKey key) = Navigation.load("/" ++ key)

type alias LootTableItem =
    { itemName : String
    , locations : List String
    }
type alias LootTable =
    { locations : List String
    , loot : List LootTableItem
    }

lootTableDecoder : Decoder LootTable
lootTableDecoder =
    Decode.map2 LootTable
        ( Decode.field "locations" (Decode.list Decode.string) )
        ( Decode.field "loot"
            <| Decode.list
            <| Decode.map2 LootTableItem
                (Decode.field "itemName" Decode.string)
                (Decode.field "locations" (Decode.list Decode.string))
        )

adminRemoveUser : RaidAdminKey -> String -> ProcessFun () msg -> Cmd msg
adminRemoveUser (RaidAdminKey key) username processFun =
    Http.post
        { url = "/api/adminRemoveUser"
        , body = Http.jsonBody
            <| Encode.object
                [ ( "raidAdminKey", Encode.string key )
                , ( "userName", Encode.string username )
                ]
        , expect = expectResponse
            processFun
            Nothing
            ( Decode.null () )
        }
