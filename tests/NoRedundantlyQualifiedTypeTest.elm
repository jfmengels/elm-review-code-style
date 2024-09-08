module NoRedundantlyQualifiedTypeTest exposing (all)

import NoRedundantlyQualifiedType exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


message : String -> String
message name =
    "This type can be simplified to just `" ++ name ++ "`."


details : String -> List String
details name =
    [ "It can be considered a bit silly to say the same word twice like in `" ++ name ++ "." ++ name ++ "`. This rule simplifies to just `" ++ name ++ "`. This follows the convention of centering modules around a type." ]


all : Test
all =
    describe "NoRedundantlyQualifiedType"
        [ test "should un-qualify a redundantly qualified type" <|
            \() ->
                """module A exposing (..)
import Set exposing (Set)
a : Set.Set a
a = Set.empty
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message "Set"
                            , details = details "Set"
                            , under = "Set.Set"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set exposing (Set)
a : Set a
a = Set.empty
"""
                        ]
        , test "should un-qualify a redundantly qualified auto-imported type" <|
            \() ->
                """module A exposing (..)
a : String.String
a = ""
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message "String"
                            , details = details "String"
                            , under = "String.String"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a : String
a = ""
"""
                        ]
        , test "should expose the type" <|
            \() ->
                """module A exposing (..)
import Set
a : Set.Set a
a = Set.empty
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message "Set"
                            , details = details "Set"
                            , under = "Set.Set"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set exposing (Set)
a : Set a
a = Set.empty
"""
                        ]
        , test "should expose the type when other things are already exposed" <|
            \() ->
                """module A exposing (..)
import Set exposing (empty)
a : Set.Set a
a = empty
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message "Set"
                            , details = details "Set"
                            , under = "Set.Set"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set exposing (Set, empty)
a : Set a
a = empty
"""
                        ]
        , test "should expose the type when other things are already exposed with unusual formatting" <|
            \() ->
                """module A exposing (..)
import Set exposing (
    empty
    )
a : Set.Set a
a = empty
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message "Set"
                            , details = details "Set"
                            , under = "Set.Set"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set exposing (
    Set, empty
    )
a : Set a
a = empty
"""
                        ]
        , test "should not report an error if another module exposes the same type name" <|
            \() ->
                """module A exposing (..)
import Set
import OtherSet exposing (Set)
a : Set.Set a
a = Set.empty
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should not report an error if another module exposes the same type name, for an auto-imported type" <|
            \() ->
                """module A exposing (..)
import OtherString exposing (String)
a : String.String
a = ""
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should not report an error a type with the same name is defined in the same file" <|
            \() ->
                """module A exposing (..)
import Set
a : Set.Set a
a = Set.empty
type Set = Set
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should not report an error a type with the same name is defined in the same file, for an auto-imported type" <|
            \() ->
                """module A exposing (..)
a : String.String
a = ""
type String = String
          """
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should not report an error if an import exposes everything including the type we want to shorten (exposing everything)" <|
            \() ->
                [ """module A exposing (..)
import Set
import OtherSet exposing (..)
a : Set.Set a
a = Set.empty
""", """module OtherSet exposing (..)

type Set a =
    Set a
""" ]
                    |> Review.Test.runOnModules rule
                    |> Review.Test.expectNoErrors
        , test "should not report an error if an import exposes everything including the type we want to shorten (exposing explicitly)" <|
            \() ->
                [ """module A exposing (..)
import Set
import OtherSet exposing (..)
a : Set.Set a
a = Set.empty
""", """module OtherSet exposing (Set)

type Set a =
    Set a
""" ]
                    |> Review.Test.runOnModules rule
                    |> Review.Test.expectNoErrors
        , test "should report an error if an import exposes everything but doesn't expose the type we want to shorten (exposing everything)" <|
            \() ->
                [ """module A exposing (..)
import Set
import SomethingElse exposing (..)
a : Set.Set a
a = Set.empty
""", """module SomethingElse exposing (..)

type NotSet a =
    NotSet a
""" ]
                    |> Review.Test.runOnModules rule
                    |> Review.Test.expect
                        [ Review.Test.moduleErrors "A"
                            [ Review.Test.error
                                { message = message "Set"
                                , details = details "Set"
                                , under = "Set.Set"
                                }
                                |> Review.Test.whenFixed """module A exposing (..)
import Set exposing (Set)
import SomethingElse exposing (..)
a : Set a
a = Set.empty
"""
                            ]
                        ]
        , test "should report an error if an import exposes everything but doesn't expose the type we want to shorten (exposing explicitly)" <|
            \() ->
                [ """module A exposing (..)
import Set
import SomethingElse exposing (..)
a : Set.Set a
a = Set.empty
""", """module SomethingElse exposing (NotSet)

type NotSet a =
    NotSet a
""" ]
                    |> Review.Test.runOnModules rule
                    |> Review.Test.expect
                        [ Review.Test.moduleErrors "A"
                            [ Review.Test.error
                                { message = message "Set"
                                , details = details "Set"
                                , under = "Set.Set"
                                }
                                |> Review.Test.whenFixed """module A exposing (..)
import Set exposing (Set)
import SomethingElse exposing (..)
a : Set a
a = Set.empty
"""
                            ]
                        ]
        , test "should report an error if an import exposes everything and defines but doesn't expose the type we want to shorten" <|
            \() ->
                [ """module A exposing (..)
import Set
import SomethingElse exposing (..)
a : Set.Set a
a = Set.empty
""", """module SomethingElse exposing (NotSet)

type NotSet a =
    NotSet a

type Set a =
    Set a
""" ]
                    |> Review.Test.runOnModules rule
                    |> Review.Test.expect
                        [ Review.Test.moduleErrors "A"
                            [ Review.Test.error
                                { message = message "Set"
                                , details = details "Set"
                                , under = "Set.Set"
                                }
                                |> Review.Test.whenFixed """module A exposing (..)
import Set exposing (Set)
import SomethingElse exposing (..)
a : Set a
a = Set.empty
"""
                            ]
                        ]
        , test "should report an error if only the correct import exposes everything" <|
            \() ->
                """module A exposing (..)
import Set exposing (..)
import OtherSet
a : Set.Set a
a = Set.empty
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message "Set"
                            , details = details "Set"
                            , under = "Set.Set"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set exposing (..)
import OtherSet
a : Set a
a = Set.empty
"""
                        ]
        , test "should report an error even if the same module exposes the same type twice" <|
            \() ->
                """module A exposing (..)
import Set exposing (Set)
import Set exposing (Set)
a : Set.Set a
a = Set.empty
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message "Set"
                            , details = details "Set"
                            , under = "Set.Set"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set exposing (Set)
import Set exposing (Set)
a : Set a
a = Set.empty
"""
                        ]
        , test "should not report an error qualified by more namespaces" <|
            \() ->
                """module A exposing (..)
import Collections.Set
a : Collections.Set.Set a
a = Collections.Set.empty
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , describe "nested types"
            [ test "type argument" <|
                \() ->
                    """module A exposing (..)
import Set exposing (Set)
a : List (Set.Set a)
a = []
"""
                        |> Review.Test.run rule
                        |> Review.Test.expectErrors
                            [ Review.Test.error
                                { message = message "Set"
                                , details = details "Set"
                                , under = "Set.Set"
                                }
                                |> Review.Test.whenFixed """module A exposing (..)
import Set exposing (Set)
a : List (Set a)
a = []
"""
                            ]
            , test "tuple" <|
                \() ->
                    """module A exposing (..)
import Set exposing (Set)
a : (Set.Set a, Int)
a = (Set.empty, 0)
"""
                        |> Review.Test.run rule
                        |> Review.Test.expectErrors
                            [ Review.Test.error
                                { message = message "Set"
                                , details = details "Set"
                                , under = "Set.Set"
                                }
                                |> Review.Test.whenFixed """module A exposing (..)
import Set exposing (Set)
a : (Set a, Int)
a = (Set.empty, 0)
"""
                            ]
            , test "triple" <|
                \() ->
                    """module A exposing (..)
import Set exposing (Set)
a : (Int, Int, Set.Set a)
a = (0, 0, Set.empty)
"""
                        |> Review.Test.run rule
                        |> Review.Test.expectErrors
                            [ Review.Test.error
                                { message = message "Set"
                                , details = details "Set"
                                , under = "Set.Set"
                                }
                                |> Review.Test.whenFixed """module A exposing (..)
import Set exposing (Set)
a : (Int, Int, Set a)
a = (0, 0, Set.empty)
"""
                            ]
            , test "record" <|
                \() ->
                    """module A exposing (..)
import Set exposing (Set)
a : { b : Set.Set a }
a = { b = Set.empty }
"""
                        |> Review.Test.run rule
                        |> Review.Test.expectErrors
                            [ Review.Test.error
                                { message = message "Set"
                                , details = details "Set"
                                , under = "Set.Set"
                                }
                                |> Review.Test.whenFixed """module A exposing (..)
import Set exposing (Set)
a : { b : Set a }
a = { b = Set.empty }
"""
                            ]
            , test "extensible record / function argument" <|
                \() ->
                    """module A exposing (..)
import Set exposing (Set)
a : { a | b : Set.Set a } -> Int
a _ = 0
"""
                        |> Review.Test.run rule
                        |> Review.Test.expectErrors
                            [ Review.Test.error
                                { message = message "Set"
                                , details = details "Set"
                                , under = "Set.Set"
                                }
                                |> Review.Test.whenFixed """module A exposing (..)
import Set exposing (Set)
a : { a | b : Set a } -> Int
a _ = 0
"""
                            ]
            , test "function return value" <|
                \() ->
                    """module A exposing (..)
import Set exposing (Set)
a : Int -> Set.Set a
a _ = Set.empty
"""
                        |> Review.Test.run rule
                        |> Review.Test.expectErrors
                            [ Review.Test.error
                                { message = message "Set"
                                , details = details "Set"
                                , under = "Set.Set"
                                }
                                |> Review.Test.whenFixed """module A exposing (..)
import Set exposing (Set)
a : Int -> Set a
a _ = Set.empty
"""
                            ]
            ]
        , describe "other definitions than functions/values"
            [ test "port" <|
                \() ->
                    """port module A exposing (..)
port a : String.String -> Cmd msg
"""
                        |> Review.Test.run rule
                        |> Review.Test.expectErrors
                            [ Review.Test.error
                                { message = message "String"
                                , details = details "String"
                                , under = "String.String"
                                }
                                |> Review.Test.whenFixed """port module A exposing (..)
port a : String -> Cmd msg
"""
                            ]
            , test "type alias" <|
                \() ->
                    """module A exposing (..)
type alias A = { a : String.String }
"""
                        |> Review.Test.run rule
                        |> Review.Test.expectErrors
                            [ Review.Test.error
                                { message = message "String"
                                , details = details "String"
                                , under = "String.String"
                                }
                                |> Review.Test.whenFixed """module A exposing (..)
type alias A = { a : String }
"""
                            ]
            , test "custom type" <|
                \() ->
                    """module A exposing (..)
type A = A String.String
"""
                        |> Review.Test.run rule
                        |> Review.Test.expectErrors
                            [ Review.Test.error
                                { message = message "String"
                                , details = details "String"
                                , under = "String.String"
                                }
                                |> Review.Test.whenFixed """module A exposing (..)
type A = A String
"""
                            ]
            ]
        , describe "duplicate imports"
            [ test "don’t update imports if one of them exposes all" <|
                \() ->
                    """module A exposing (..)
import Set
import Set exposing (..)
a : Set.Set a
a = Set.empty
"""
                        |> Review.Test.run rule
                        |> Review.Test.expectErrors
                            [ Review.Test.error
                                { message = message "Set"
                                , details = details "Set"
                                , under = "Set.Set"
                                }
                                |> Review.Test.whenFixed """module A exposing (..)
import Set
import Set exposing (..)
a : Set a
a = Set.empty
"""
                            ]
            , test "add the type to the first import when none expose anything" <|
                \() ->
                    """module A exposing (..)
import Set
import Set
a : Set.Set a
a = Set.empty
"""
                        |> Review.Test.run rule
                        |> Review.Test.expectErrors
                            [ Review.Test.error
                                { message = message "Set"
                                , details = details "Set"
                                , under = "Set.Set"
                                }
                                |> Review.Test.whenFixed """module A exposing (..)
import Set exposing (Set)
import Set
a : Set a
a = Set.empty
"""
                            ]
            ]
        ]
