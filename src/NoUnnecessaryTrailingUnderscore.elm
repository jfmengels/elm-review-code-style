module NoUnnecessaryTrailingUnderscore exposing (rule)

{-|

@docs rule

-}

import Dict exposing (Dict)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Exposing as Exposing
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Module as Module
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range exposing (Range)
import Review.Fix as Fix exposing (Fix)
import Review.Project.Dependency as Dependency exposing (Dependency)
import Review.Rule as Rule exposing (Rule)
import Set exposing (Set)


{-| Reports unnecessary or suboptimal trailing underscores in variable names.

🔧 Running with `--fix` will automatically remove most of the reported errors.

    config =
        [ NoUnnecessaryTrailingUnderscore.rule
        ]

I don't know how widespread this usage is, but I tend to append variable names with `_` to avoid [shadowing conflicts](https://github.com/elm/compiler/blob/9d97114702bf6846cab622a2203f60c2d4ebedf2/hints/shadowing.md), for instance like this:

    viewName name =
      case name of
        Just name_ -> ...
        Nothing -> ...

Obviously when I am able to figure out a better name for one of the two variables, I go for that. In this case, I may rename the argument to `viewName` to `maybeName` for instance.

But I notice that relatively often, the need for having the trailing underscore disappears, because I changed some code and the variable name that required me to add a trailing underscore has also disappeared. When that happens, the trailing underscore becomes a distraction and I find it nicer to rename the variable to not have that underscore.

This rule does not propose a fix for the issues (at least for now). I recommend renaming the variables through your IDE, as that will be a simpler and safer process.
That said, as we'll see in the following sections and examples, these renames may end up in shadowing issues, so please do these renames with a compiler running next to (or `elm-test --watch` for the changes happening in test files) to notice problems early on.


## Fail


### 1. Unneeded trailing underscore

When a variable has a trailing underscore that could be removed without an issue.

    viewName maybeName =
        case maybeName of
            Just name_ ->
                name_


### 2. Top-level declarations containing an underscore

This rule reports top-level declarations containing an underscore, even when removing the underscore would cause a shadowing conflict.
The idea here is that if you have both `viewName` and `viewName_`, someone who looks at the code may have a hard time figuring which function they should use and what the differences are between the two.

    viewName_ name = ...

When `viewName_` is only used inside `viewName`, an alternative name that you could go for is appending `Help`, such as `viewNameHelp`, which I've seen regularly often in several packages and codebases.


### 3. Let declarations on the same level where one has an underscore and one doesn't

    a =
        let
            name =
                "Jeroen"

            name_ =
                "Engels"
        in
        name ++ " " ++ name_

Very similar to the previous point, we report `name_` because this name is too confusing in the sense that readers won't know which one to use when.
In such instances, I believe there are clear benefits from spending a little bit of time figuring out a better name.

Here is another instance where a better name could be given.

    a =
        let
            model =
                { a = 1, b = 2 }

            model_ =
                { model | b = model.b + 1 }
        in
        doSomething model_

In this example, even simple naming schemes like `initialModel` and `modelWithUpdatedB`, or `modelStep1` and `modelStep2` would be an improvement over a trailing underscore. I'm sure you can find even better names for your specific use-case!


### 4. When an underscore is used to avoid a shadowing conflict with a more deeply nested variable

    view model_ =
        case model_ of
            Loaded model ->
                text model.name

In this case, `model_` has a trailing underscore to avoid a conflict with `model` declared in a deeper scope.
I tend to find constructs like these to be an indication that either

  - one or both of those variables have a bad name for what they're represent (`model` could be `loadedModel`?)
  - if they represent the same type, again we have an issue where it's not clear when one should be used over the other


## Success

We don't report errors when there is a reasonable use-case for adding a trailing underscore, or when a variable does not have a trailing underscore.

    viewName name =
        case name of
            Just name_ ->
                name_


## When (not) to enable this rule

This is a pretty personal rule that I'd like to enforce on my own projects.
I have not yet tested it extensively, but I feel like it could bring some value and bring me some sense of satisfaction when I know that the project adheres to this rule.

I feel comfortable enough with asking people making pull requests to the projects I maintain to make changes in order to follow this rule.
But I will probably not enforce this rule in a project where my team is bigger, because this may be too big of a source of frustration for my colleagues, especially if they tend to notice problems in the CI and no way to autofix the issues.

I recommend AGAINST enforcing this rule if you do not agree with the choices I have made, or if you do not have that habit of adding trailing underscores.
If you see some value in it, you may still want to use this rule to detect places where naming could be improved and make improvements to these places, but not end up enforcing it.


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template jfmengels/elm-review-code-style/example --rules NoUnnecessaryTrailingUnderscore
```

-}
rule : Rule
rule =
    Rule.newProjectRuleSchema "NoUnnecessaryTrailingUnderscore" initialProjectContext
        |> Rule.withDirectDependenciesProjectVisitor dependenciesVisitor
        |> Rule.withModuleVisitor moduleVisitor
        |> Rule.withModuleContextUsingContextCreator
            { fromProjectToModule = fromProjectToModule
            , fromModuleToProject = fromModuleToProject
            , foldProjectContexts = foldProjectContexts
            }
        |> Rule.withContextFromImportedModules
        |> Rule.fromProjectRuleSchema


dependenciesVisitor : Dict String Dependency -> ProjectContext -> ( List nothing, ProjectContext )
dependenciesVisitor dependencies projectContext =
    ( []
    , dependencies
        |> Dict.values
        |> List.concatMap Dependency.modules
        |> List.foldl
            (\{ name, values } acc ->
                Dict.insert (String.split "." name) (List.map .name values) acc
            )
            projectContext
    )


moduleVisitor : Rule.ModuleRuleSchema schemaState ModuleContext -> Rule.ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor schema =
    schema
        |> Rule.withImportVisitor importVisitor
        |> Rule.withDeclarationListVisitor declarationListVisitor
        |> Rule.withDeclarationEnterVisitor declarationVisitor
        |> Rule.withExpressionEnterVisitor expressionVisitor
        |> Rule.withExpressionExitVisitor expressionExitVisitor


type alias ProjectContext =
    Dict ModuleName (List String)


initialProjectContext : ProjectContext
initialProjectContext =
    Dict.empty


type alias ModuleContext =
    { scopes : Scopes
    , scopesToAdd : Dict RangeLike Scope
    , exposedFunctionsFromModule : Dict ModuleName (List String)
    }


type alias Scopes =
    ( Scope, List Scope )


type alias Scope =
    Dict String (List Range)


type alias RangeLike =
    List Int


fromProjectToModule : Rule.ContextCreator ProjectContext ModuleContext
fromProjectToModule =
    Rule.initContextCreator
        (\projectContext ->
            { scopes = ( importedFromBasics, [] )
            , scopesToAdd = Dict.empty
            , exposedFunctionsFromModule = projectContext
            }
        )


fromModuleToProject : Rule.ContextCreator ModuleContext ProjectContext
fromModuleToProject =
    Rule.initContextCreator
        (\moduleName ast _ ->
            Dict.singleton moduleName (collectExposed ast)
        )
        |> Rule.withModuleName
        |> Rule.withFullAst


collectExposed : File -> List String
collectExposed ast =
    case Node.value ast.moduleDefinition |> Module.exposingList of
        Exposing.Explicit list ->
            List.filterMap
                (\node ->
                    case Node.value node of
                        Exposing.FunctionExpose name ->
                            Just name

                        _ ->
                            Nothing
                )
                list

        Exposing.All _ ->
            List.filterMap
                (\decl ->
                    case Node.value decl of
                        Declaration.FunctionDeclaration { declaration } ->
                            Just (Node.value (Node.value declaration).name)

                        _ ->
                            Nothing
                )
                ast.declarations


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts new previous =
    Dict.union new previous


importVisitor : Node Import -> ModuleContext -> ( List nothing, ModuleContext )
importVisitor (Node _ import_) context =
    case Maybe.map Node.value import_.exposingList of
        Just (Exposing.Explicit list) ->
            let
                namesToAdd : Scope
                namesToAdd =
                    List.filterMap
                        (\node ->
                            case Node.value node of
                                Exposing.FunctionExpose name ->
                                    Just ( name, [] )

                                _ ->
                                    Nothing
                        )
                        list
                        |> Dict.fromList
            in
            ( []
            , { context | scopes = Tuple.mapFirst (mergeScopeDicts namesToAdd) context.scopes }
            )

        Just (Exposing.All _) ->
            case Dict.get (Node.value import_.moduleName) context.exposedFunctionsFromModule of
                Just names ->
                    let
                        namesToAdd : Scope
                        namesToAdd =
                            List.map (\valueName -> ( valueName, [] )) names
                                |> Dict.fromList
                    in
                    ( []
                    , { context | scopes = Tuple.mapFirst (mergeScopeDicts namesToAdd) context.scopes }
                    )

                Nothing ->
                    ( [], context )

        Nothing ->
            ( [], context )


declarationListVisitor : List (Node Declaration) -> ModuleContext -> ( List (Rule.Error {}), ModuleContext )
declarationListVisitor declarations context =
    let
        namesToAdd : Scope
        namesToAdd =
            List.filterMap
                (\node ->
                    case Node.value node of
                        Declaration.FunctionDeclaration function ->
                            Just
                                ( function.declaration
                                    |> Node.value
                                    |> .name
                                    |> Node.value
                                , []
                                )

                        _ ->
                            Nothing
                )
                declarations
                |> Dict.fromList

        errors : List (Rule.Error {})
        errors =
            List.filterMap
                (\node ->
                    case Node.value node of
                        Declaration.FunctionDeclaration function ->
                            reportTopLevelFunction context.scopes function

                        _ ->
                            Nothing
                )
                declarations
    in
    ( errors
    , { context | scopes = Tuple.mapFirst (mergeScopeDicts namesToAdd) context.scopes }
    )


mergeScopeDicts : Scope -> Scope -> Scope
mergeScopeDicts scopeA scopeB =
    Dict.merge
        Dict.insert
        (\key valuesA valuesB -> Dict.insert key (valuesA ++ valuesB))
        Dict.insert
        scopeA
        scopeB
        Dict.empty


declarationVisitor : Node Declaration -> ModuleContext -> ( List (Rule.Error {}), ModuleContext )
declarationVisitor node context =
    case Node.value node of
        Declaration.FunctionDeclaration function ->
            let
                arguments : List (Node Pattern)
                arguments =
                    function.declaration
                        |> Node.value
                        |> .arguments

                body : Node Expression
                body =
                    function.declaration
                        |> Node.value
                        |> .expression
            in
            report
                [ ( arguments, body ) ]
                context

        _ ->
            ( [], context )


type alias ScopeNames =
    { name : String
    , range : Range
    , signatureRange : Maybe Range
    , origin : NameOrigin
    }


type NameOrigin
    = FromRecord
    | NotFromRecord


getDeclaredVariableNames : Node Pattern -> List ScopeNames
getDeclaredVariableNames pattern =
    case Node.value pattern of
        Pattern.VarPattern name ->
            [ { name = name, range = Node.range pattern, signatureRange = Nothing, origin = NotFromRecord } ]

        Pattern.ParenthesizedPattern subPattern ->
            getDeclaredVariableNames subPattern

        Pattern.AsPattern subPattern name ->
            { name = Node.value name, range = Node.range name, signatureRange = Nothing, origin = NotFromRecord } :: getDeclaredVariableNames subPattern

        Pattern.TuplePattern patterns ->
            List.concatMap getDeclaredVariableNames patterns

        Pattern.UnConsPattern left right ->
            List.concatMap getDeclaredVariableNames [ left, right ]

        Pattern.ListPattern patterns ->
            List.concatMap getDeclaredVariableNames patterns

        Pattern.NamedPattern _ patterns ->
            List.concatMap getDeclaredVariableNames patterns

        Pattern.RecordPattern fields ->
            List.map (\field -> { name = Node.value field, range = Node.range field, signatureRange = Nothing, origin = FromRecord }) fields

        _ ->
            []


reservedElmKeywords : Set String
reservedElmKeywords =
    Set.fromList
        [ "if_"
        , "then_"
        , "else_"
        , "case_"
        , "of_"
        , "let_"
        , "in_"
        , "type_"
        , "module_"
        , "where_"
        , "import_"
        , "exposing_"
        , "as_"
        , "port_"

        -- Until `elm-format` and `elm-syntax` allow `infix` as an identifier
        , "infix_"
        ]


expressionVisitor : Node Expression -> ModuleContext -> ( List (Rule.Error {}), ModuleContext )
expressionVisitor node context =
    let
        newContext : ModuleContext
        newContext =
            case Dict.get (Node.range node |> rangeToRangeLike) context.scopesToAdd of
                Just scopeToAdd ->
                    { context | scopes = addNewScope scopeToAdd context.scopes }

                Nothing ->
                    context
    in
    expressionVisitorHelp node newContext


expressionExitVisitor : Node Expression -> ModuleContext -> ( List nothing, ModuleContext )
expressionExitVisitor node context =
    let
        newContext : ModuleContext
        newContext =
            if Dict.member (Node.range node |> rangeToRangeLike) context.scopesToAdd then
                { context | scopes = popScope context.scopes }

            else
                context
    in
    ( [], newContext )


expressionVisitorHelp : Node Expression -> ModuleContext -> ( List (Rule.Error {}), ModuleContext )
expressionVisitorHelp node context =
    case Node.value node of
        Expression.CaseExpression { cases } ->
            report
                (List.map (Tuple.mapFirst List.singleton) cases)
                context

        Expression.LambdaExpression { args, expression } ->
            report
                [ ( args, expression ) ]
                context

        Expression.LetExpression { declarations, expression } ->
            let
                namesFromLetDeclarations : Scope
                namesFromLetDeclarations =
                    getNamesFromLetDeclarations declarations
                        |> Dict.fromList

                ( errors, newContext ) =
                    report
                        (List.concatMap
                            (\declaration ->
                                case Node.value declaration of
                                    Expression.LetFunction function ->
                                        let
                                            functionImplementation : Expression.FunctionImplementation
                                            functionImplementation =
                                                Node.value function.declaration
                                        in
                                        [ ( functionImplementation.arguments
                                          , functionImplementation.expression
                                          )
                                        ]

                                    Expression.LetDestructuring _ _ ->
                                        []
                            )
                            declarations
                        )
                        { context | scopes = addNewScope namesFromLetDeclarations context.scopes }
            in
            ( reportErrorsForLet node namesFromLetDeclarations context.scopes declarations ++ errors
            , { newContext
                | scopesToAdd =
                    Dict.insert
                        (rangeToRangeLike (Node.range expression))
                        namesFromLetDeclarations
                        newContext.scopesToAdd
              }
            )

        _ ->
            ( [], context )


getNamesFromLetDeclarations : List (Node Expression.LetDeclaration) -> List ( String, List Range )
getNamesFromLetDeclarations declarations =
    List.concatMap
        (\declaration ->
            case Node.value declaration of
                Expression.LetFunction function ->
                    let
                        name : Node String
                        name =
                            function.declaration
                                |> Node.value
                                |> .name
                    in
                    [ ( Node.value name
                      , List.filterMap
                            identity
                            [ Just (Node.range name)
                            , Maybe.map (Node.value >> .name >> Node.range) function.signature
                            ]
                      )
                    ]

                Expression.LetDestructuring pattern _ ->
                    List.map .name (getDeclaredVariableNames pattern)
                        |> List.map (\name -> ( name, [] ))
        )
        declarations


reportErrorsForLet : Node Expression -> Scope -> Scopes -> List (Node Expression.LetDeclaration) -> List (Rule.Error {})
reportErrorsForLet letExpression namesFromLetDeclarations scopes declarations =
    List.concatMap
        (\node ->
            case Node.value node of
                Expression.LetFunction function ->
                    let
                        functionName : Node String
                        functionName =
                            function.declaration
                                |> Node.value
                                |> .name
                    in
                    case
                        error
                            letExpression
                            namesFromLetDeclarations
                            scopes
                            { name = Node.value functionName
                            , range = Node.range functionName
                            , signatureRange = Maybe.map (\(Node _ signature) -> Node.range signature.name) function.signature
                            , origin = NotFromRecord
                            }
                    of
                        Just newError ->
                            [ newError ]

                        Nothing ->
                            []

                Expression.LetDestructuring pattern _ ->
                    let
                        declaredVariables : List ScopeNames
                        declaredVariables =
                            getDeclaredVariableNames pattern

                        names : Scope
                        names =
                            declaredVariables
                                |> List.map (\var -> ( var.name, [] ))
                                |> Dict.fromList
                    in
                    List.filterMap (error letExpression namesFromLetDeclarations (addNewScope names scopes)) declaredVariables
        )
        declarations


reportTopLevelFunction : Scopes -> Expression.Function -> Maybe (Rule.Error {})
reportTopLevelFunction scopes function =
    let
        functionNameNode : Node String
        functionNameNode =
            function.declaration
                |> Node.value
                |> .name

        functionName : String
        functionName =
            Node.value functionNameNode

        functionNameWithoutUnderscore : String
        functionNameWithoutUnderscore =
            String.dropRight 1 functionName
    in
    if
        String.endsWith "_" functionName
            && not (isDefinedInScope scopes functionNameWithoutUnderscore)
            && not (Set.member functionName reservedElmKeywords)
    then
        Just
            (Rule.error
                { message = "Top-level declaration names should not end with an underscore"
                , details =
                    [ "A trailing underscore \"_\" is often used to prevent shadowing issues, but top-level declarations should not resolve these issues in that manner."
                    ]
                }
                (Node.range functionNameNode)
            )

    else
        Nothing


report : List ( List (Node Pattern), Node Expression ) -> ModuleContext -> ( List (Rule.Error {}), ModuleContext )
report patternsAndBody context =
    let
        scopesToAdd : List { errors : List (Rule.Error {}), scopesToAdd : ( RangeLike, Scope ) }
        scopesToAdd =
            List.map
                (\( patterns, expression ) ->
                    let
                        declaredVariables : List ScopeNames
                        declaredVariables =
                            List.concatMap getDeclaredVariableNames patterns

                        names : Scope
                        names =
                            declaredVariables
                                |> List.map (\var -> ( var.name, [] ))
                                |> Dict.fromList
                    in
                    { errors = List.filterMap (error expression Dict.empty (addNewScope names context.scopes)) declaredVariables
                    , scopesToAdd =
                        ( rangeToRangeLike (Node.range expression)
                        , names
                        )
                    }
                )
                patternsAndBody
    in
    ( List.concatMap .errors scopesToAdd
    , { context
        | scopesToAdd =
            Dict.union
                (scopesToAdd |> List.map .scopesToAdd |> Dict.fromList)
                context.scopesToAdd
      }
    )


addNewScope : Scope -> Scopes -> Scopes
addNewScope scope ( head, tail ) =
    ( scope, head :: tail )


popScope : Scopes -> Scopes
popScope ( head, tail ) =
    case tail of
        [] ->
            ( head, tail )

        newHead :: newTail ->
            ( newHead, newTail )


error : Node Expression -> Scope -> Scopes -> ScopeNames -> Maybe (Rule.Error {})
error expressionToReplaceThingsIn namesOnTheSameLevel scopes { range, name, signatureRange, origin } =
    let
        nameWithoutUnderscore : String
        nameWithoutUnderscore =
            String.dropRight 1 name
    in
    if
        String.endsWith "_" name
            && not (isDefinedInScope scopes nameWithoutUnderscore)
            && not (Set.member name reservedElmKeywords)
            && shouldNameBeReported origin
    then
        if Dict.member nameWithoutUnderscore namesOnTheSameLevel then
            Just
                (Rule.error
                    { message = name ++ " should not end with an underscore"
                    , details =
                        [ "It seems that it has been used to prevent shadowing issues with a variable on the same level, but this is confusing. When should \"" ++ name ++ "\" be used and when should \"" ++ nameWithoutUnderscore ++ "\" be used?"
                        , "Please rename this variable in a way that makes it more explicit when or how each should be used."
                        ]
                    }
                    range
                )

        else
            Just
                (Rule.errorWithFix
                    (defaultErrorAndMessage name)
                    range
                    (case findUsages name nameWithoutUnderscore [ expressionToReplaceThingsIn ] [] of
                        Just usages ->
                            List.map removeLastCharacter (addMaybe signatureRange (range :: usages))

                        Nothing ->
                            []
                    )
                )

    else
        Nothing


addMaybe : Maybe a -> List a -> List a
addMaybe maybe list =
    case maybe of
        Just element ->
            element :: list

        Nothing ->
            list


findUsages : String -> String -> List (Node Expression) -> List Range -> Maybe (List Range)
findUsages targetName intendedName remainingNodes acc =
    case remainingNodes of
        [] ->
            Just acc

        node :: rest ->
            case Node.value node of
                Expression.FunctionOrValue [] name ->
                    if name == intendedName then
                        Nothing

                    else if name == targetName then
                        findUsages targetName intendedName rest (Node.range node :: acc)

                    else
                        findUsages targetName intendedName rest acc

                Expression.RecordUpdateExpression (Node nameRange name) nodes ->
                    if name == targetName then
                        findUsages targetName intendedName (List.map (Node.value >> Tuple.second) nodes ++ rest) (nameRange :: acc)

                    else
                        findUsages targetName intendedName (List.map (Node.value >> Tuple.second) nodes ++ rest) acc

                Expression.Application nodes ->
                    findUsages targetName intendedName (nodes ++ rest) acc

                Expression.OperatorApplication _ _ left right ->
                    findUsages targetName intendedName (left :: right :: rest) acc

                Expression.IfBlock cond then_ else_ ->
                    findUsages targetName intendedName (cond :: then_ :: else_ :: rest) acc

                Expression.Negation expr ->
                    findUsages targetName intendedName (expr :: rest) acc

                Expression.TupledExpression nodes ->
                    findUsages targetName intendedName (nodes ++ rest) acc

                Expression.ParenthesizedExpression expr ->
                    findUsages targetName intendedName (expr :: rest) acc

                Expression.LetExpression letBlock ->
                    findUsages targetName intendedName (letBlock.expression :: (List.map extractExpression letBlock.declarations ++ rest)) acc

                Expression.CaseExpression caseBlock ->
                    findUsages targetName intendedName (caseBlock.expression :: (List.map Tuple.second caseBlock.cases ++ rest)) acc

                Expression.LambdaExpression lambda ->
                    findUsages targetName intendedName (lambda.expression :: rest) acc

                Expression.RecordExpr nodes ->
                    findUsages targetName intendedName (List.map (Node.value >> Tuple.second) nodes ++ rest) acc

                Expression.ListExpr nodes ->
                    findUsages targetName intendedName (nodes ++ rest) acc

                Expression.RecordAccess expr _ ->
                    findUsages targetName intendedName (expr :: rest) acc

                _ ->
                    findUsages targetName intendedName rest acc


extractExpression : Node Expression.LetDeclaration -> Node Expression
extractExpression letExpr =
    case Node.value letExpr of
        Expression.LetFunction { declaration } ->
            (Node.value declaration).expression

        Expression.LetDestructuring _ expression ->
            expression


removeLastCharacter : Range -> Fix
removeLastCharacter range =
    Fix.removeRange
        { start = { row = range.end.row, column = range.end.column - 1 }
        , end = range.end
        }


defaultErrorAndMessage : String -> { message : String, details : List String }
defaultErrorAndMessage name =
    { message = name ++ " should not end with an underscore"
    , details =
        [ "It looks like this was used to avoid a shadowing issue, but the variable it would have clashed with is not present in the scope of where this variable was declared anymore. You should rename the variable and remove the underscore."
        , "Note that this may not be a safe change, in that renaming may clash with a value declared deeper in the expression, but I think it's less confusing to have the nested variable have a trailing underscore rather than the variable declared higher up."
        ]
    }


shouldNameBeReported : NameOrigin -> Bool
shouldNameBeReported origin =
    case origin of
        FromRecord ->
            False

        NotFromRecord ->
            True


isDefinedInScope : Scopes -> String -> Bool
isDefinedInScope ( top, rest ) name =
    List.any (Dict.member name) (top :: rest)


rangeToRangeLike : Range -> RangeLike
rangeToRangeLike range =
    [ range.start.row
    , range.start.column
    , range.end.row
    , range.end.column
    ]


importedFromBasics : Scope
importedFromBasics =
    [ "toFloat"
    , "round"
    , "floor"
    , "ceiling"
    , "truncate"
    , "max"
    , "min"
    , "compare"
    , "not"
    , "xor"
    , "modBy"
    , "remainderBy"
    , "negate"
    , "abs"
    , "clamp"
    , "sqrt"
    , "logBase"
    , "e"
    , "pi"
    , "cos"
    , "sin"
    , "tan"
    , "acos"
    , "asin"
    , "atan"
    , "atan2"
    , "degrees"
    , "radians"
    , "turns"
    , "toPolar"
    , "fromPolar"
    , "isNaN"
    , "isInfinite"
    , "identity"
    , "always"
    , "never"
    ]
        |> List.map (\name -> ( name, [] ))
        |> Dict.fromList
