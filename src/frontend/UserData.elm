module UserData exposing (UserData, userDataDecoder, userDataEncoder)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode

type alias UserData =
    { userName : String
    , class : String
    , role : String
    , prio1 : Maybe String
    , prio2 : Maybe String
    }

userDataEncoder : String -> UserData -> Encode.Value
userDataEncoder raidID data =
    Encode.object
        [ ( "userName", Encode.string data.userName )
        , ( "userKey", Encode.string raidID )
        , ( "class", Encode.string data.class )
        , ( "role", Encode.string data.role )
        , ( "prio1", Maybe.withDefault Encode.null <| Maybe.map Encode.string data.prio1 )
        , ( "prio2", Maybe.withDefault Encode.null <| Maybe.map Encode.string data.prio2 )
        ]

userDataDecoder : Decoder UserData
userDataDecoder =
    Decode.map5 UserData
        (Decode.field "userName" Decode.string)
        (Decode.field "class" Decode.string)
        (Decode.field "role" Decode.string)
        (Decode.field "prio1" (Decode.nullable Decode.string))
        (Decode.field "prio2" (Decode.nullable Decode.string))