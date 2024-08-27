module NoRedundantlyQualifiedType exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Declaration as Declaration exposing (Declaration(..))
import Elm.Syntax.Expression as Expression exposing (Expression, LetBlock, LetDeclaration(..))
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation(..))
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (Rule)


{-| Reports when a type is qualified by a module (alias) of the same name.

🔧 Running with `--fix` will automatically remove most of the reported errors.

    config =
        [ NoRedundantlyQualifiedType.rule
        ]

There is a convention in Elm to center modules around a type, and to name the module after the type. Usually, such a module is imported like this:

    import Inventory.Fruit as Fruit exposing (Fruit)

That allows you to refer to values like `Fruit.apple` and to the type `Fruit`. When inferring types, you might end up with `Fruit.Fruit`. It can be considered a bit silly to say the same word twice like that. It's clear where the `Fruit` type comes from anyway. This rule simplifies to just `Fruit`.


## Fail

    import Set

    directions : Set.Set String
    directions =
        Set.fromList [ "north", "south", "east", "west" ]


## Success

    import Set exposing (Set)

    directions : Set String
    directions =
        Set.fromList [ "north", "south", "east", "west" ]


## When (not) to enable this rule

This rule resolves a minor style issue, and may not be worth enforcing depending on how strongly you feel about this issue.


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template jfmengels/elm-review-code-style/example --rules NoRedundantlyQualifiedType
```

-}
rule : Rule
rule =
    Rule.newModuleRuleSchemaUsingContextCreator "NoRedundantlyQualifiedType" initialContext
        |> Rule.withDeclarationEnterVisitor declarationVisitor
        |> Rule.withLetDeclarationEnterVisitor letDeclarationEnterVisitor
        |> Rule.withFinalModuleEvaluation finalEvaluation
        |> Rule.fromModuleRuleSchema


type alias Context =
    { lookupTable : ModuleNameLookupTable }


initialContext : Rule.ContextCreator () Context
initialContext =
    Rule.initContextCreator
        (\lookupTable () -> { lookupTable = lookupTable })
        |> Rule.withModuleNameLookupTable


declarationVisitor : Node Declaration -> Context -> ( List (Rule.Error {}), Context )
declarationVisitor node context =
    case Node.value node of
        FunctionDeclaration function ->
            doFunction function context

        AliasDeclaration typeAlias ->
            ä

        CustomTypeDeclaration type_ ->
            ä

        PortDeclaration signature ->
            ä

        InfixDeclaration _ ->
            ( [], context )

        Destructuring _ _ ->
            ( [], context )


letDeclarationEnterVisitor : Node Expression.LetBlock -> Node Expression.LetDeclaration -> Context -> ( List (Rule.Error {}), Context )
letDeclarationEnterVisitor _ letDeclaration context =
    case Node.value letDeclaration of
        LetFunction function ->
            doFunction function context

        LetDestructuring _ _ ->
            ( [], context )


finalEvaluation : Context -> List (Rule.Error {})
finalEvaluation context =
    Debug.todo "declarationVisitor"


doFunction function context =
    case function.signature of
        Just (Node _ signature) ->
            doTypeAnnotation signature.typeAnnotation
                |> ä

        Nothing ->
            ( [], context )


doTypeAnnotation typeAnnotation context =
    case Node.value typeAnnotation of
        GenericType _ ->
            ( [], context )

        Typed constructor arguments ->
            let
                foo =
                    case constructor of
                        Node _ ( [ qualifier ], name ) ->
                            if qualifier == name then
                                -- Also check lookup table:
                                -- Need to know which import to check and maybe update
                                -- Need to look up the type name to see that it's free
                                [ Rule.error "Redundantly qualified type" typeAnnotation ]

                            else
                                []

                        _ ->
                            []
            in
            -- Now visit the arguments
            -- Add test for `List (Set.Set a)`
            ä

        Unit ->
            ( [], context )

        Tupled arguments ->
            -- Now visit the arguments
            -- Add tests for tuples and triples
            ä

        Record recordDefinition ->
            -- Visit the things in the record
            -- Add test for records
            ä

        GenericRecord _ (Node _ recordDefinition) ->
            -- Visit the things in the record
            -- Add test for generic records
            ä

        FunctionTypeAnnotation left right ->
            -- Visit the left and right
            -- Add test for function types
            ä
