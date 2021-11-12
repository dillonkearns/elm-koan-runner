# `elm-koan-runner` [![Build Status](https://github.com/dillonkearns/elm-koan-runner/workflows/CI/badge.svg)](https://github.com/dillonkearns/elm-koan-runner/actions?query=branch%3Amain)

Thanks to Robert Looby and the other contributors to the [`elm-koans`](https://github.com/robertjlooby/elm-koans) project. This package was originally forked from the `elm-koans` test runner.

This project helps you run koan-style Elm exercises.


```elm
module Main exposing (main)


import Expect
import Test.Koan exposing (Test, describe, test)


main =
    describe "About Expects"
        [ -- `test` takes the test description and a function that returns an
          -- `Expectation` when evaluated with the unit tuple `()`
          test "Expect.true tests for a true value"
            (\() -> Expect.true "Should be True" (x____replace me____x))

        -- `<|` calls the function on the left with the argument on the right
        --  and can be used to avoid some parenthesis
        , test "Expect.equal tests for equality" <|
            \() -> Expect.equal True (x____replace me____x)

        -- `|>` calls the function on the right with the argument on the left
        -- and can be used to "pipeline" values through a series of functions
        , test "Expect.notEqual tests for inequality" <|
            \() ->
                x____replace me____x
                    |> Expect.notEqual False
        ]
        |> Test.Koan.program


type FILL_ME_IN
    = Blank


me____x : FILL_ME_IN
me____x =
    Blank


x____replace : FILL_ME_IN -> a
x____replace _ =
    Debug.todo "FILL IN THE BLANK"
```
