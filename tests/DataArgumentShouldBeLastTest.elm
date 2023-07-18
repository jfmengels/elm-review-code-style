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
type Msg = Msg
type Model = Model
update model msg =
    model
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should not report an error when the data is last" <|
            \() ->
                """module A exposing (..)
type Msg = Msg
type Model = Model
update : Msg -> Model -> Model
update msg model =
    model
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should report an error when the return type is present in the arguments but not as the last one" <|
            \() ->
                """module A exposing (..)
type Msg = Msg
type Model = Model
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
                            |> Review.Test.atExactly { start = { row = 4, column = 10 }, end = { row = 4, column = 15 } }
                        ]
        , test "should not report an error when the return type is not in the arguments" <|
            \() ->
                """module A exposing (..)
type Msg = Msg
type Model = Model
type OtherThing = OtherThing
fn : OtherThing -> Msg -> Model
fn otherThing msg =
    {}
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should not report an error when the data is there multiple times but also as the last argument" <|
            \() ->
                """module A exposing (..)
type Msg = Msg
type Model = Model
add : Model -> Model -> Model
add a b =
    a + b
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should report an error when the return type is in arguments but not last, but the type is not defined in the same file" <|
            \() ->
                """module A exposing (..)
import Model exposing (Model)
type Msg = Msg
update : Model -> Msg -> Model
update model msg =
    model
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        ]
