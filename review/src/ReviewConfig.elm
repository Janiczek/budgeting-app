module ReviewConfig exposing (config)

import AllTextareasWithGrammarlyDisabled
import ConsistentImports
import NoEmptyHtmlText
import NoExposingEverything
import NoImportingEverything
import NoLeftPizza
import NoRedundantCons
import NoUnused.CustomTypeConstructors
import NoUnused.Dependencies
import NoUnused.Exports
import NoUnused.Modules
import NoUnused.Parameters
import NoUnused.Patterns
import NoUnused.Variables
import Review.Rule as Rule exposing (Rule)
import Simplify


config : List Rule
config =
    [ NoUnused.CustomTypeConstructors.rule []
    , NoUnused.Dependencies.rule
    , NoUnused.Exports.rule
    , NoUnused.Modules.rule
    , NoUnused.Parameters.rule
    , NoUnused.Patterns.rule
    , NoUnused.Variables.rule
    , AllTextareasWithGrammarlyDisabled.rule
    , ConsistentImports.rule
    , NoLeftPizza.rule NoLeftPizza.Redundant
    , NoEmptyHtmlText.rule
    , Simplify.rule Simplify.defaults
    , NoExposingEverything.rule
    , NoImportingEverything.rule []
    , NoRedundantCons.rule
    ]
