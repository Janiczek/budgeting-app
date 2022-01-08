module NoEmptyHtmlText exposing (rule)

{-| Make sure we use Html.nothing instead of Html.text ""

To be used with <https://package.elm-lang.org/packages/jfmengels/elm-review/latest/>


# Rule

@docs rule

-}

import Elm.Syntax.Expression exposing (Expression(..))
import Elm.Syntax.Node as Node exposing (Node)
import Review.Rule as Rule exposing (Error, Rule)


{-| Make sure we use Html.nothing instead of Html.text ""
If you want to use this rule, add it to `config : List Rule` in `review/ReviewConfig.elm`
-}
rule : Rule
rule =
    Rule.newModuleRuleSchema "EmptyHtmlTextUsed" ()
        |> Rule.withSimpleExpressionVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema


expressionVisitor : Node Expression -> List (Error {})
expressionVisitor node =
    case Node.value node of
        Application (fn :: arg :: _) ->
            case ( Node.value fn, Node.value arg ) of
                ( FunctionOrValue [ "Html" ] "text", Literal "" ) ->
                    [ Rule.error
                        { message = "Html.text \"\" should not be used"
                        , details =
                            [ "Please use Html.Extra.nothing instead!"
                            , "Preferably `import Html.Extra as Html` and then `Html.nothing`"
                            ]
                        }
                        (Node.range node)
                    ]

                _ ->
                    []

        _ ->
            []
