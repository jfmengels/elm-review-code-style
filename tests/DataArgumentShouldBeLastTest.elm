module DataArgumentShouldBeLastTest exposing (all)

import DataArgumentShouldBeLast exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "DataArgumentShouldBeLast"
        [ test "should not report an error when there is no type annotation" <|
            \() ->
                """module A exposing (main)
type Msg = Msg
type Model = Model
update model msg =
    model
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should not report an error when the data is last" <|
            \() ->
                """module A exposing (main)
type Msg = Msg
type Model = Model
update : Msg -> Model -> Model
update msg model =
    model
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should not report an error when the data is last and also present as a different argument" <|
            \() ->
                """module A exposing (main)
type Msg = Msg
type Model = Model
update : Msg -> Model -> Model -> Model
update msg model1 model2 =
    model2
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should report an error when the return type is present in the arguments but not as the last one" <|
            \() ->
                """module A exposing (main)
type Msg = Msg
type Model = Model
update : Model -> Msg -> Model
update model msg =
    model

value = update model msg
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The data argument should be last"
                            , details =
                                [ "In Elm, it is common in functions that return the same type as one of the arguments to have that argument be the last. This makes it for instance easy to compose operations using `|>` or `>>`."
                                , "Example: instead of `update : Model -> Msg -> Model`, it is more idiomatic to have `update : Msg -> Model -> Model`"
                                ]
                            , under = "Model"
                            }
                            |> Review.Test.atExactly { start = { row = 4, column = 10 }, end = { row = 4, column = 15 } }
                            |> Review.Test.whenFixed """module A exposing (main)
type Msg = Msg
type Model = Model
update : Msg -> Model -> Model
update msg model =
    model

value = update msg model
"""
                        ]
        , test "should report an error when the return type is present in the arguments but not as the last one (multi-line)" <|
            \() ->
                """module A exposing (main)
type Msg = Msg
type Model = Model
update :
    Model
    -> Msg
    -> Model
update model msg =
    model

value =
    update
        msg
        model

value2 =
    update msg
        model
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The data argument should be last"
                            , details =
                                [ "In Elm, it is common in functions that return the same type as one of the arguments to have that argument be the last. This makes it for instance easy to compose operations using `|>` or `>>`."
                                , "Example: instead of `update : Model -> Msg -> Model`, it is more idiomatic to have `update : Msg -> Model -> Model`"
                                ]
                            , under = "Model"
                            }
                            |> Review.Test.atExactly { start = { row = 5, column = 5 }, end = { row = 5, column = 10 } }
                            |> Review.Test.whenFixed """module A exposing (main)
type Msg = Msg
type Model = Model
update :
    Msg
    -> Model
    -> Model
update msg model =
    model

value =
    update
        model
        msg

value2 =
    update model
        msg
"""
                        ]
        , test "should not expect a fix if not all arguments in the declaration" <|
            \() ->
                """module A exposing (main)
type Msg = Msg
type Model = Model
update :
    Model
    -> Msg
    -> Model
update model =
    fn
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The data argument should be last"
                            , details =
                                [ "In Elm, it is common in functions that return the same type as one of the arguments to have that argument be the last. This makes it for instance easy to compose operations using `|>` or `>>`."
                                , "Example: instead of `update : Model -> Msg -> Model`, it is more idiomatic to have `update : Msg -> Model -> Model`"
                                ]
                            , under = "Model"
                            }
                            |> Review.Test.atExactly { start = { row = 5, column = 5 }, end = { row = 5, column = 10 } }
                        ]
        , test "should not expect a fix if the function is exposed" <|
            \() ->
                """module A exposing (update)
type Msg = Msg
type Model = Model
update :
    Model
    -> Msg
    -> Model
update model msg =
    model
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The data argument should be last"
                            , details =
                                [ "In Elm, it is common in functions that return the same type as one of the arguments to have that argument be the last. This makes it for instance easy to compose operations using `|>` or `>>`."
                                , "Example: instead of `update : Model -> Msg -> Model`, it is more idiomatic to have `update : Msg -> Model -> Model`"
                                ]
                            , under = "Model"
                            }
                            |> Review.Test.atExactly { start = { row = 5, column = 5 }, end = { row = 5, column = 10 } }
                        ]
        , test "should not report an error when the return type is not in the arguments" <|
            \() ->
                """module A exposing (main)
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
                """module A exposing (main)
type Msg = Msg
type Model = Model
add : Model -> Model -> Model
add a b =
    a + b
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should not report an error when the return type is in arguments but not last, but the type is not defined in the same file" <|
            \() ->
                """module A exposing (main)
import Model exposing (Model)
type Msg = Msg
update : Model -> Msg -> Model
update model msg =
    model
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , -- TODO We should check whether we can/should report this
          test "should not report an error when the type variables are different for the argument and the return type" <|
            \() ->
                """module A exposing (main)
type X a = X a
map : X a -> (a -> b) -> X b
update model msg =
    model
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        ]
