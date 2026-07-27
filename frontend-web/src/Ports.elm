port module Ports exposing
    ( configLoaded
    , journalChanged
    , promptInstall
    , promptInstallReady
    , registerServiceWorker
    , skipWaiting
    , storeConfig
    , swStatus
    )

import Json.Decode
import Json.Encode


port storeConfig : Json.Encode.Value -> Cmd msg


port configLoaded : (Json.Decode.Value -> msg) -> Sub msg


port registerServiceWorker : () -> Cmd msg


port swStatus : (String -> msg) -> Sub msg


port journalChanged : (String -> msg) -> Sub msg


port promptInstallReady : (() -> msg) -> Sub msg


port promptInstall : () -> Cmd msg


port skipWaiting : () -> Cmd msg
