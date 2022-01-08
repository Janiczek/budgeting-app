module ConsistentImports exposing (rule)

import Dict exposing (Dict)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Review.Rule as Rule exposing (Error, Rule)


rule : Rule
rule =
    Rule.newModuleRuleSchema "ConsistentImports" ()
        |> Rule.withSimpleImportVisitor importVisitor
        |> Rule.fromModuleRuleSchema


allowedAliases : Dict ModuleName (List ModuleName)
allowedAliases =
    [ ( "Json.Decode", [ "Decode" ] )
    , ( "Json.Encode", [ "Encode" ] )
    , ( "Html.Attributes", [ "Attrs" ] )
    , ( "Gwi.Html.Attributes", [ "Attrs" ] )
    , ( "Html.Attributes.Extra"
      , [ "Attrs"
        , "Attrs_" -- if using `autocomplete` there is a conflict, and this is a way out of it
        ]
      )
    , ( "Html.Events", [ "Events" ] )
    , ( "Gwi.Html.Events", [ "Events" ] )
    , ( "Html.Events.Extra", [ "Events" ] )
    , ( "Gwi.RemoteData", [ "RemoteData" ] )
    , ( "List.Extra", [ "List" ] )
    ]
        |> List.map toModuleNames
        |> Dict.fromList


toModuleName : String -> ModuleName
toModuleName joined =
    String.split "." joined


fromModuleName : ModuleName -> String
fromModuleName moduleName =
    String.join "." moduleName


toModuleNames : ( String, List String ) -> ( ModuleName, List ModuleName )
toModuleNames ( original, allowedAliases_ ) =
    ( toModuleName original
    , List.map toModuleName allowedAliases_
    )


importVisitor : Node Import -> List (Error {})
importVisitor importNode =
    let
        imp : Import
        imp =
            Node.value importNode

        original : ModuleName
        original =
            Node.value imp.moduleName
    in
    imp.moduleAlias
        |> Maybe.map
            (\moduleAliasNode ->
                let
                    moduleAlias : ModuleName
                    moduleAlias =
                        Node.value moduleAliasNode
                in
                case Dict.get original allowedAliases of
                    Nothing ->
                        []

                    Just allowedAliases_ ->
                        if List.any ((==) moduleAlias) allowedAliases_ then
                            []

                        else
                            [ error importNode allowedAliases_ ]
            )
        -- no alias is OK
        |> Maybe.withDefault []


error : Node Import -> List ModuleName -> Error {}
error importNode allowedAliases_ =
    let
        imp : Import
        imp =
            Node.value importNode

        original : ModuleName
        original =
            Node.value imp.moduleName
    in
    Rule.error
        { message = "Bad alias for " ++ fromModuleName original ++ " import"
        , details =
            "Use the following form for consistency"
                :: List.map (allowedImportLine original) allowedAliases_
        }
        (Node.range importNode)


allowedImportLine : ModuleName -> ModuleName -> String
allowedImportLine original allowedAlias =
    "import "
        ++ fromModuleName original
        ++ " as "
        ++ fromModuleName allowedAlias
