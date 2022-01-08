module AllTextareasWithGrammarlyDisabled exposing (rule)

{-| All `Html.textareas` should use `Attrs.withDisabledGrammarly`
somewhere in their first argument (the attributes list).

To be used with <https://package.elm-lang.org/packages/jfmengels/elm-review/latest/>


# Rule

@docs rule

-}

import Elm.Syntax.Expression exposing (Expression(..))
import Elm.Syntax.Node as Node exposing (Node)
import Review.Rule as Rule exposing (Error, Rule)


rule : Rule
rule =
    Rule.newModuleRuleSchema "AllTextareasWithGrammarlyDisabled" ()
        |> Rule.withSimpleExpressionVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema


expressionVisitor : Node Expression -> List (Error {})
expressionVisitor node =
    case Node.value node of
        Application (fn :: arg :: _) ->
            case ( Node.value fn, Node.value arg ) of
                ( FunctionOrValue [ "Html" ] "textarea", ListExpr attrs ) ->
                    if
                        attrs
                            |> List.any
                                (\attr ->
                                    case Node.value attr of
                                        FunctionOrValue [ "Attrs" ] "withDisabledGrammarly" ->
                                            True

                                        _ ->
                                            False
                                )
                    then
                        []

                    else
                        [ Rule.error
                            { message = "Html.textarea should disable Grammarly"
                            , details =
                                [ "All `Html.textareas` should use `Attrs.withDisabledGrammarly` somewhere in their first argument (the attributes list)."
                                , "You can do so by adding `|> Attrs.withDisabledGrammarly` at the end of the attribute list."
                                ]
                            }
                            (Node.range node)
                        ]

                _ ->
                    []

        _ ->
            []
