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
                        ]
        ]
