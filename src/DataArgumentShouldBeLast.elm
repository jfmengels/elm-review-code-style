module DataArgumentShouldBeLast exposing (rule)

{-|

@docs rule

-}

import Dict exposing (Dict)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Exposing as Exposing exposing (Exposing)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Module as Module
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern)
import Elm.Syntax.Range as Range exposing (Location, Range)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (RecordField, TypeAnnotation)
import Review.Fix as Fix exposing (Fix)
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
        |> Rule.providesFixesForProjectRule
        |> Rule.fromProjectRuleSchema


type alias ProjectContext =
    {}


type alias ModuleContext =
    { isExposed : String -> Bool
    , lookupTable : ModuleNameLookupTable
    , extractSourceCode : Range -> String
    , errors : Dict String PendingError
    , rangeToIgnore : Maybe Range
    }


type alias PendingError =
    { range : Range
    , nbOfArguments : Int
    , indexOfMovedArgument : Int
    , fixes : List Fix
    }


moduleVisitor : Rule.ModuleRuleSchema schema ModuleContext -> Rule.ModuleRuleSchema { schema | hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor schema =
    schema
        |> Rule.withDeclarationListVisitor declarationListVisitor
        |> Rule.withExpressionEnterVisitor expressionVisitor
        |> Rule.withFinalModuleEvaluation finalEvaluation


initialProjectContext : ProjectContext
initialProjectContext =
    {}


fromProjectToModule : Rule.ContextCreator ProjectContext ModuleContext
fromProjectToModule =
    Rule.initContextCreator
        (\lookupTable extractSourceCode ast projectContext ->
            let
                exposing_ : Exposing
                exposing_ =
                    Module.exposingList (Node.value ast.moduleDefinition)
            in
            { lookupTable = lookupTable
            , extractSourceCode = extractSourceCode
            , errors = Dict.empty
            , isExposed = \name -> Exposing.exposesFunction name exposing_
            , rangeToIgnore = Nothing
            }
        )
        |> Rule.withModuleNameLookupTable
        |> Rule.withSourceCodeExtractor
        |> Rule.withFullAst


fromModuleToProject : Rule.ContextCreator ModuleContext ProjectContext
fromModuleToProject =
    Rule.initContextCreator
        (\moduleContext ->
            {}
        )


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts new previous =
    {}


declarationListVisitor : List (Node Declaration) -> ModuleContext -> ( List (Rule.Error {}), ModuleContext )
declarationListVisitor nodes context =
    List.foldl
        (\node ( previousErrors, previousContext ) ->
            let
                ( newErrors, newContext ) =
                    declarationVisitor node previousContext
            in
            ( newErrors ++ previousErrors, newContext )
        )
        ( [], context )
        nodes


declarationVisitor : Node Declaration -> ModuleContext -> ( List (Rule.Error {}), ModuleContext )
declarationVisitor node context =
    case Node.value node of
        Declaration.FunctionDeclaration { signature, declaration } ->
            case signature of
                Just (Node _ type_) ->
                    case isNotDataLast type_.typeAnnotation context.lookupTable of
                        Just { argPosition, argIndex, nextArgumentRange, nbOfArguments, returnType } ->
                            ( []
                            , { context
                                | errors =
                                    Dict.insert
                                        (Node.value (Node.value declaration).name)
                                        { range = argPosition
                                        , nbOfArguments = nbOfArguments
                                        , indexOfMovedArgument = argIndex
                                        , fixes =
                                            if context.isExposed (Node.value (Node.value declaration).name) then
                                                []

                                            else
                                                createFix
                                                    context
                                                    nbOfArguments
                                                    argPosition
                                                    argIndex
                                                    nextArgumentRange
                                                    returnType
                                                    (Node.range (Node.value declaration).name).end
                                                    (Node.value declaration).arguments
                                        }
                                        context.errors
                              }
                            )

                        Nothing ->
                            ( [], context )

                Nothing ->
                    ( [], context )

        _ ->
            ( [], context )


expressionVisitor : Node Expression -> ModuleContext -> ( List (Rule.Error {}), ModuleContext )
expressionVisitor node context =
    case Node.value node of
        Expression.Application ((Node fnRange (Expression.FunctionOrValue [] name)) :: arguments) ->
            case Dict.get name context.errors of
                Just error ->
                    case ModuleNameLookupTable.moduleNameFor context.lookupTable node of
                        Just [] ->
                            let
                                cancelFixesAndReportError : () -> ( List (Rule.Error {}), ModuleContext )
                                cancelFixesAndReportError () =
                                    ( [ createError { range = error.range, fixes = [] } ]
                                    , { context | errors = Dict.remove name context.errors }
                                    )
                            in
                            if List.length arguments == error.nbOfArguments then
                                case createFixesForFunctionCall context error fnRange arguments of
                                    [] ->
                                        cancelFixesAndReportError ()

                                    newFixes ->
                                        ( []
                                        , { context
                                            | rangeToIgnore = Just fnRange
                                            , errors = Dict.insert name { error | fixes = newFixes ++ error.fixes } context.errors
                                          }
                                        )

                            else
                                cancelFixesAndReportError ()

                        _ ->
                            ( [], context )

                Nothing ->
                    ( [], context )

        Expression.FunctionOrValue [] name ->
            if context.rangeToIgnore == Just (Node.range node) then
                ( [], { context | rangeToIgnore = Nothing } )

            else
                case Dict.get name context.errors of
                    Just error ->
                        case ModuleNameLookupTable.moduleNameFor context.lookupTable node of
                            Just [] ->
                                ( [ createError { range = error.range, fixes = [] } ]
                                , { context | errors = Dict.remove name context.errors }
                                )

                            _ ->
                                ( [], context )

                    Nothing ->
                        ( [], context )

        _ ->
            ( [], context )


createFixesForFunctionCall : ModuleContext -> PendingError -> Range -> List (Node Expression) -> List Fix
createFixesForFunctionCall context error fnNameLocation arguments =
    case getLastArgAt error.nbOfArguments arguments of
        Just { end } ->
            case listAtIndex error.indexOfMovedArgument arguments fnNameLocation.end of
                Just range ->
                    moveCode context
                        { from = range
                        , to = end
                        }

                Nothing ->
                    []

        Nothing ->
            []


finalEvaluation : ModuleContext -> List (Rule.Error {})
finalEvaluation moduleContext =
    Dict.foldl
        (\_ error errors ->
            createError error :: errors
        )
        []
        moduleContext.errors


createError : { a | range : Range, fixes : List Fix } -> Rule.Error {}
createError { range, fixes } =
    Rule.errorWithFix
        { message = "The data argument should be last"
        , details =
            [ "In Elm, it is common in functions that return the same type as one of the arguments to have that argument be the last. This makes it for instance easy to compose operations using `|>` or `>>`."
            , "Example: instead of `update : Model -> Msg -> Model`, it is more idiomatic to have `update : Msg -> Model -> Model`"
            ]
        }
        range
        fixes


createFix : ModuleContext -> Int -> Range -> Int -> Range -> Node d -> Location -> List (Node Pattern) -> List Fix
createFix context nbOfArguments argPosition argIndex nextArgumentRange returnType fnNameEndLocation arguments =
    case Maybe.map2 Tuple.pair (listAtIndex argIndex arguments fnNameEndLocation) (getLastArgAt nbOfArguments arguments) of
        Just ( rangeToMove, lastArgPosition ) ->
            List.concat
                [ moveCode context
                    { from = { start = argPosition.start, end = nextArgumentRange.start }
                    , to = (Node.range returnType).start
                    }
                , moveCode context
                    { from = rangeToMove
                    , to = lastArgPosition.end
                    }
                ]

        Nothing ->
            []


moveCode : ModuleContext -> { from : Range, to : Location } -> List Fix
moveCode context { from, to } =
    [ Fix.removeRange from
    , Fix.insertAt to (context.extractSourceCode from)
    ]


isNotDataLast : Node TypeAnnotation -> ModuleNameLookupTable -> Maybe { argPosition : Range, argIndex : Int, nextArgumentRange : Range, nbOfArguments : Int, returnType : Node TypeAnnotation }
isNotDataLast type_ lookupTable =
    case getArguments type_ lookupTable [] of
        Nothing ->
            Nothing

        Just { returnType, arguments } ->
            case arguments of
                firstArg :: rest ->
                    if Node.value firstArg == Node.value returnType then
                        Nothing

                    else
                        case findAndGiveElementAndItsPrevious (\arg -> Node.value arg == Node.value returnType) 0 firstArg rest of
                            Just ( arg, argIndex, nextElement ) ->
                                Just
                                    { argPosition = Node.range arg
                                    , argIndex = argIndex
                                    , nextArgumentRange = Node.range nextElement
                                    , nbOfArguments = List.length rest + 1
                                    , returnType = returnType
                                    }

                            Nothing ->
                                Nothing

                [] ->
                    Nothing


{-| Returned arguments are in the opposite order.
-}
getArguments : Node TypeAnnotation -> ModuleNameLookupTable -> List (Node TypeAnnotation) -> Maybe { returnType : Node TypeAnnotation, arguments : List (Node TypeAnnotation) }
getArguments type_ lookupTable argsAcc =
    case Node.value type_ of
        TypeAnnotation.FunctionTypeAnnotation arg return_ ->
            getArguments return_ lookupTable ((Node (Node.range arg) <| Node.value <| removeRange arg) :: argsAcc)

        TypeAnnotation.Typed _ _ ->
            case ModuleNameLookupTable.moduleNameFor lookupTable type_ of
                Just [] ->
                    Just
                        { returnType = Node (Node.range type_) <| Node.value <| removeRange type_
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


findAndGiveElementAndItsPrevious : (a -> Bool) -> Int -> a -> List a -> Maybe ( a, Int, a )
findAndGiveElementAndItsPrevious predicate index previous list =
    case list of
        [] ->
            Nothing

        x :: xs ->
            if predicate x then
                Just ( x, index, previous )

            else
                findAndGiveElementAndItsPrevious predicate (index + 1) x xs


listAtIndex : Int -> List (Node a) -> Location -> Maybe Range
listAtIndex index list prevPosition =
    case list of
        [] ->
            Nothing

        x :: xs ->
            if index == 0 then
                Just { start = prevPosition, end = (Node.range x).end }

            else
                listAtIndex (index - 1) xs (Node.range x).end


getLastArgAt : Int -> List (Node a) -> Maybe Range
getLastArgAt nbOfArguments arguments =
    case List.drop (nbOfArguments - 1) arguments of
        x :: [] ->
            Just (Node.range x)

        _ ->
            Nothing
