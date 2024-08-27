module NoRedundantlyQualifiedTypeTest exposing (all)

import NoRedundantlyQualifiedType exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


messageForSet =
    "This type can be simplified to just `Set`."


detailsForSet =
    [ "It can be considered a bit silly to say the same word twice like in `Set.Set`. This rule simplifies to just `Set`. It's clear where the `Set` type comes from anyway." ]


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
                            { message = messageForSet
                            , details = detailsForSet
                            , under = "Set.Set"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set exposing (Set)
a : Set a
a = Set.empty
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
                            { message = messageForSet
                            , details = detailsForSet
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
                            { message = messageForSet
                            , details = detailsForSet
                            , under = "Set.Set"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set exposing (Set, empty)
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
        , test "should not report an error qualified by more namespaces" <|
            \() ->
                """module A exposing (..)
import Collections.Set
a : Collections.Set.Set a
a = Collections.Set.empty
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        ]
