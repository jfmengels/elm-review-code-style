module DataArgumentShouldBeLast exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import Review.Rule as Rule exposing (Rule)


{-| Reports... REPLACEME

    config =
        [ DataArgumentShouldBeLast.rule
        ]


## Fail

    a =
        "REPLACEME example to replace"


## Success

    a =
        "REPLACEME example to replace"


## When (not) to enable this rule

This rule is useful when REPLACEME.
This rule is not useful when REPLACEME.


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template jfmengels/elm-review-code-style/example --rules DataArgumentShouldBeLast
```

-}
rule : Rule
rule =
    Rule.newProjectRuleSchema "DataArgumentShouldBeLast" initialProjectContext
        |> Rule.withModuleVisitor moduleVisitor
        |> Rule.withModuleContextUsingContextCreator
            { fromProjectToModule = fromProjectToModule
            , fromModuleToProject = fromModuleToProject
            , foldProjectContexts = foldProjectContexts
            }
        -- Enable this if modules need to get information from other modules
        -- |> Rule.withContextFromImportedModules
        |> Rule.fromProjectRuleSchema


type alias ProjectContext =
    {}


type alias ModuleContext =
    {}


moduleVisitor : Rule.ModuleRuleSchema schema ModuleContext -> Rule.ModuleRuleSchema { schema | hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor schema =
    schema
        |> Rule.withDeclarationEnterVisitor declarationVisitor


initialProjectContext : ProjectContext
initialProjectContext =
    {}


fromProjectToModule : Rule.ContextCreator ProjectContext ModuleContext
fromProjectToModule =
    Rule.initContextCreator
        (\projectContext ->
            {}
        )


fromModuleToProject : Rule.ContextCreator ModuleContext ProjectContext
fromModuleToProject =
    Rule.initContextCreator
        (\moduleContext ->
            {}
        )


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts new previous =
    {}


declarationVisitor : Node Declaration -> ProjectContext -> ( List (Rule.Error {}), ProjectContext )
declarationVisitor node context =
    case Node.value node of
        Declaration.FunctionDeclaration { signature, declaration } ->
            case signature of
                Just (Node _ type_) ->
                    case isNotDataLast type_.typeAnnotation of
                        Just { dataPosition, returnType } ->
                            ( [ Rule.error
                                    { message = "REPLACEME"
                                    , details = [ "REPLACEME" ]
                                    }
                                    (Node.range node)
                              ]
                            , context
                            )

                        Nothing ->
                            ( [], context )

                Nothing ->
                    ( [], context )

        _ ->
            ( [], context )


isNotDataLast : Node TypeAnnotation -> Maybe b
isNotDataLast type_ =
    let
        { returnType } =
            getReturnType type_ []
    in
    Nothing


getReturnType : Node TypeAnnotation -> List (Node TypeAnnotation) -> { returnType : TypeAnnotation }
getReturnType type_ argsAcc =
    case Node.value type_ of
        TypeAnnotation.FunctionTypeAnnotation arg return_ ->
            getReturnType return_ (arg :: argsAcc)

        _ ->
            { returnType = Node.value type_
            }
