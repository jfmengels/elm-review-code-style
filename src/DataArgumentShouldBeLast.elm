module DataArgumentShouldBeLast exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range as Range exposing (Range)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (RecordField, TypeAnnotation)
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (Rule)


{-| Reports functions where the data argument is not the last argument.

    config =
        [ DataArgumentShouldBeLast.rule
        ]

This rule will report functions that return the same type as one of the arguments when that argument is not the last.

The idea is to help make idiomatic Elm code, that makes it easy to compose code. For example, given the following functions:

    newBicycle : Bicycle

    withNumberOfGears : Int -> Bicycle -> Bicycle

    withTire : Tire -> Bicycle -> Bicycle

    withDutchBicycleLock : Bicycle -> Bicycle

because the data that is being altered/modified is the last one, we can easily compose these functions using `|>`:

    myBicycle : Bicycle
    myBicycle =
        newBicycle
            |> withNumberOfGears 7
            |> withTire mountainBikeTire
            |> withDutchBicycleLock

or using `>>` :

    turnIntoMountainBike =
        withNumberOfGears 7
            >> withTire mountainBikeTire
            >> withDutchBicycleLock

If instead, we had `withNumberOfGears : Bicycle -> Int -> Bicycle`, this would not compose as well.

In Elm code, because of this property, we often write our functions data-last in case we need to compose them with other ones.


## Fail

    update : Model -> Msg -> Model
    update model msg =
        -- ...


## Success

    update : Msg -> Model -> Model
    update msg model =
        -- ...


## Boundaries of the rule

To avoid false positives, this rule only applies in some conditions.

REPLACEME


## When (not) to enable this rule

This rule can help make your Elm code more idiomatic.

If you feel like this rule reports false positives, please open an issue.


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
    { lookupTable : ModuleNameLookupTable
    }


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
        (\lookupTable projectContext ->
            { lookupTable = lookupTable
            }
        )
        |> Rule.withModuleNameLookupTable


fromModuleToProject : Rule.ContextCreator ModuleContext ProjectContext
fromModuleToProject =
    Rule.initContextCreator
        (\moduleContext ->
            {}
        )


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts new previous =
    {}


declarationVisitor : Node Declaration -> ModuleContext -> ( List (Rule.Error {}), ModuleContext )
declarationVisitor node context =
    case Node.value node of
        Declaration.FunctionDeclaration { signature, declaration } ->
            case signature of
                Just (Node _ type_) ->
                    case isNotDataLast type_.typeAnnotation context.lookupTable of
                        Just { argPosition, returnType } ->
                            ( [ Rule.error
                                    { message = "The data argument should be last"
                                    , details =
                                        [ "In Elm, it is common in functions that return the same type as one of the arguments to have that argument be the last. This makes it for instance easy to compose operations using `|>` or `>>`."
                                        , "Example: instead of `update : Model -> Msg -> Model`, it is more idiomatic to have `update : Msg -> Model -> Model`"
                                        ]
                                    }
                                    argPosition
                              ]
                            , context
                            )

                        Nothing ->
                            ( [], context )

                Nothing ->
                    ( [], context )

        _ ->
            ( [], context )


isNotDataLast : Node TypeAnnotation -> ModuleNameLookupTable -> Maybe { argPosition : Range, returnType : TypeAnnotation }
isNotDataLast type_ lookupTable =
    case getArguments type_ lookupTable [] of
        Nothing ->
            Nothing

        Just { returnType, arguments } ->
            case arguments of
                firstArg :: rest ->
                    if Node.value firstArg == returnType then
                        Nothing

                    else
                        case find (\arg -> Node.value arg == returnType) rest of
                            Just arg ->
                                Just
                                    { argPosition = Node.range arg
                                    , returnType = returnType
                                    }

                            Nothing ->
                                Nothing

                [] ->
                    Nothing


{-| Returned arguments are in the opposite order.
-}
getArguments : Node TypeAnnotation -> ModuleNameLookupTable -> List (Node TypeAnnotation) -> Maybe { returnType : TypeAnnotation, arguments : List (Node TypeAnnotation) }
getArguments type_ lookupTable argsAcc =
    case Node.value type_ of
        TypeAnnotation.FunctionTypeAnnotation arg return_ ->
            getArguments return_ lookupTable ((Node (Node.range arg) <| Node.value <| removeRange arg) :: argsAcc)

        TypeAnnotation.Typed _ _ ->
            case ModuleNameLookupTable.moduleNameFor lookupTable type_ of
                Just [] ->
                    Just
                        { returnType = Node.value (removeRange type_)
                        , arguments = argsAcc
                        }

                _ ->
                    Nothing

        _ ->
            Nothing


removeRange : Node TypeAnnotation -> Node TypeAnnotation
removeRange node =
    case Node.value node of
        TypeAnnotation.GenericType string ->
            Node Range.emptyRange (TypeAnnotation.GenericType string)

        TypeAnnotation.Typed (Node _ qualifier) nodes ->
            Node Range.emptyRange (TypeAnnotation.Typed (Node Range.emptyRange qualifier) (List.map removeRange nodes))

        TypeAnnotation.Unit ->
            node

        TypeAnnotation.Tupled nodes ->
            Node Range.emptyRange (TypeAnnotation.Tupled (List.map removeRange nodes))

        TypeAnnotation.Record recordDefinition ->
            Node Range.emptyRange (TypeAnnotation.Record (List.map removeRangeFromRecordField recordDefinition))

        TypeAnnotation.GenericRecord (Node _ var) (Node _ recordDefinition) ->
            Node Range.emptyRange (TypeAnnotation.GenericRecord (Node Range.emptyRange var) (Node Range.emptyRange (List.map removeRangeFromRecordField recordDefinition)))

        TypeAnnotation.FunctionTypeAnnotation input output ->
            Node Range.emptyRange (TypeAnnotation.FunctionTypeAnnotation (removeRange input) (removeRange output))


removeRangeFromRecordField : Node RecordField -> Node RecordField
removeRangeFromRecordField (Node _ ( Node _ property, value )) =
    Node Range.emptyRange ( Node Range.emptyRange property, removeRange value )


find : (a -> Bool) -> List a -> Maybe a
find predicate list =
    case list of
        [] ->
            Nothing

        x :: xs ->
            if predicate x then
                Just x

            else
                find predicate xs
