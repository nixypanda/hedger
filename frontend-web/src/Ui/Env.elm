module Ui.Env exposing (Env, resize, withTheme, withViewport)

import Element exposing (Device)
import Ui.Theme exposing (Theme)
import Ui.Viewport exposing (Viewport)


type alias Env =
    { theme : Theme
    , device : Device
    , viewport : Viewport
    }


withTheme : Env -> Theme -> Env
withTheme env theme =
    { env | theme = theme }


withViewport : Env -> Viewport -> Env
withViewport env viewport =
    { env
        | viewport = viewport
        , device = Element.classifyDevice viewport
    }


resize : Env -> Int -> Int -> Env
resize env width height =
    withViewport env { width = width, height = height }
