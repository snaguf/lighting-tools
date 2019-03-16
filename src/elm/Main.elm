module Main exposing (main)

import Browser
import LightingTools


main : Program () LightingTools.Model LightingTools.Msg
main =
    Browser.document
        { init = \flags -> LightingTools.init
        , view = \model -> { title = model.title, body = [ LightingTools.view model ] }
        , update = LightingTools.update
        , subscriptions = LightingTools.subscriptions
        }
