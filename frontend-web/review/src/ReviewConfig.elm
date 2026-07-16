module ReviewConfig exposing (config)

import NoDebug.Log
import NoDebug.TodoOrToString
import NoExposingEverything
import NoInconsistentAliases
import NoMissingSubscriptionsCall
import NoMissingTypeAnnotation
import NoModuleOnExposedNames
import NoRecursiveUpdate
import NoRedundantlyQualifiedType
import NoSimpleLetBody
import NoUnnecessaryTrailingUnderscore
import NoUnused.Dependencies
import NoUnused.Patterns
import NoUnused.Variables
import NoUselessSubscriptions
import Review.Rule exposing (Rule)
import Simplify


config : List Rule
config =
    [ NoDebug.Log.rule
    , NoDebug.TodoOrToString.rule
        |> Review.Rule.ignoreErrorsForFiles [ "tests/TestDate.elm" ]
        |> Review.Rule.ignoreErrorsForFiles [ "tests/TestAccountName.elm" ]
    , NoExposingEverything.rule
    , NoInconsistentAliases.config
        [ ( "Browser.Navigation", "Nav" )
        , ( "Element.Background", "Background" )
        , ( "Element.Border", "Border" )
        , ( "Element.Font", "Font" )
        , ( "Element.Input", "Input" )
        , ( "Element.Region", "Region" )
        , ( "Json.Decode", "Decode" )
        , ( "Json.Encode", "Encode" )
        , ( "Url.Builder", "Builder" )
        , ( "Url.Parser", "Parser" )
        , ( "Url.Parser.Query", "Query" )
        ]
        |> NoInconsistentAliases.rule
    , NoMissingTypeAnnotation.rule
    , NoModuleOnExposedNames.rule
    , NoRedundantlyQualifiedType.rule
    , NoUnused.Dependencies.rule
    , NoUnused.Patterns.rule
    , NoSimpleLetBody.rule
    , NoUnnecessaryTrailingUnderscore.rule
    , NoUnused.Variables.rule
    , Simplify.rule Simplify.defaults
    , NoMissingSubscriptionsCall.rule
    , NoRecursiveUpdate.rule
    , NoUselessSubscriptions.rule
    ]
