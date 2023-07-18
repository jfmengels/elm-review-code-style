module DataArgumentShouldBeLastTest exposing (all)

import DataArgumentShouldBeLast exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "DataArgumentShouldBeLast"
        [ test "should not report an error when there is no type annotation" <|
            \() ->
                """module A exposing (..)
update model msg =
    model
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should not report an error when the data is last" <|
            \() ->
                """module A exposing (..)
update : Msg -> Model -> Model
update msg model =
    model
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should report an error when the data is not last" <|
            \() ->
                """module A exposing (..)
update : Model -> Msg -> Model
update model msg =
    model
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "REPLACEME"
                            , details = [ "REPLACEME" ]
                            , under = "Model"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 10 }, end = { row = 2, column = 15 } }
                        ]
        , test "should not report an error when the return type is not in the arguments" <|
            \() ->
                """module A exposing (..)
fn : OtherThing -> Msg -> Model
fn otherThing msg =
    {}
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should not report an error when the data is there multiple times but also as the last argument" <|
            \() ->
                """module A exposing (..)
add : a -> a -> a
add a b =
    a + b
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        ]
