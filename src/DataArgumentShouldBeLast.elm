module DataArgumentShouldBeLast exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range as Range exposing (Range)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (RecordField, TypeAnnotation)
import Review.Rule as Rule exposing (Rule)
import Set exposing (Set)


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
    { localTypes : Set String
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
        (\ast projectContext ->
            { localTypes = getTypeNames ast.declarations Set.empty
            }
        )
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
    ( [], { localTypes = Set.empty } )


getTypeNames : List (Node Declaration) -> Set String -> Set String
getTypeNames nodes acc =
    case nodes of
        [] ->
            acc

        node :: rest ->
            case getTypeName node of
                Just name ->
                    getTypeNames rest (Set.insert name acc)

                Nothing ->
                    getTypeNames rest acc


getTypeName : Node Declaration -> Maybe String
getTypeName node =
    case Node.value node of
        Declaration.CustomTypeDeclaration { name } ->
            Just (Node.value name)

        Declaration.AliasDeclaration { name } ->
            Just (Node.value name)

        _ ->
            Nothing


declarationVisitor : Node Declaration -> ModuleContext -> ( List (Rule.Error {}), ModuleContext )
declarationVisitor node context =
    case Node.value node of
        Declaration.FunctionDeclaration { signature, declaration } ->
            case signature of
                Just (Node _ type_) ->
                    case isNotDataLast type_.typeAnnotation of
                        Just { argPosition, returnType } ->
                            ( [ Rule.error
                                    { message = "REPLACEME"
                                    , details = [ "REPLACEME" ]
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


isNotDataLast : Node TypeAnnotation -> Maybe { argPosition : Range, returnType : TypeAnnotation }
isNotDataLast type_ =
    let
        { returnType, arguments } =
            getReturnType type_ []
    in
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
getReturnType : Node TypeAnnotation -> List (Node TypeAnnotation) -> { returnType : TypeAnnotation, arguments : List (Node TypeAnnotation) }
getReturnType type_ argsAcc =
    case Node.value type_ of
        TypeAnnotation.FunctionTypeAnnotation arg return_ ->
            getReturnType return_ ((Node (Node.range arg) <| Node.value <| removeRange arg) :: argsAcc)

        _ ->
            { returnType = Node.value (removeRange type_)
            , arguments = argsAcc
            }


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
