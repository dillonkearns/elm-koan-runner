module Test.Koan exposing
    ( program
    , Test, describe, test
    )

{-| Koan

@docs program


## elm-test helpers

These are all drop-in replacements for the helpers you'll find in `elm-test`.
`elm-koan-runner` takes each one and streams failures so only one failure at a time is displayed.

@docs Test, describe, test

-}

import Browser
import Browser.Dom
import Expect
import Html exposing (Html, b, div, h1, node, pre, progress, span, text)
import Html.Attributes exposing (class, style, title, value)
import Process
import Task
import Test as ElmTest
import Test.Runner as ElmTestRunner
import Test.Runner.Failure exposing (Reason, format)


{-| An Elm Program that presents failing tests one at a time in the style of a Koan exercise.
-}
program : Test -> Program () Model Msg
program koans =
    Browser.element
        { init = \() -> init koans
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }



-- KOANS


{-| A test case or group of test cases.
-}
type Test
    = Batch String (List Test)
    | Single String (() -> Expect.Expectation)


{-| Groups together a List of Tests.
-}
describe : String -> List Test -> Test
describe =
    Batch


{-| Creates a single test case.
-}
test : String -> (() -> Expect.Expectation) -> Test
test =
    Single



-- RUNNING TESTS


type alias Failure =
    { given : Maybe String
    , description : String
    , reason : Reason
    }


type Event
    = Section String
    | Run String (() -> Maybe Failure)


asStream : List Test -> List Event
asStream tests =
    case tests of
        [] ->
            []

        (Batch description subTests) :: next ->
            Section description :: asStream (subTests ++ next)

        (Single description thunk) :: next ->
            Run description (thunk >> ElmTestRunner.getFailureReason) :: asStream next


asCanonical : Test -> ElmTest.Test
asCanonical aTest =
    case aTest of
        Batch description children ->
            ElmTest.describe description (List.map asCanonical children)

        Single description thunk ->
            ElmTest.test description thunk



-- PROGRAM


type alias Model =
    { seen : List Event
    , upcoming : List Event
    , final : Final
    }


type Final
    = InProgress
    | Finished
    | Failed Failure


type Msg
    = Step
    | Fail Failure


init : Test -> ( Model, Cmd Msg )
init koans =
    ( { seen = []
      , upcoming = asStream [ koans ]
      , final = InProgress
      }
    , Browser.Dom.getViewport
        |> Task.andThen (.scene >> .height >> Browser.Dom.setViewport 0)
        |> Task.perform (\_ -> Step)
    )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.upcoming ) of
        ( Fail error, _ ) ->
            ( { model | final = Failed error }, Cmd.none )

        ( Step, [] ) ->
            ( { model | final = Finished }, Cmd.none )

        ( Step, event :: rest ) ->
            ( { model | upcoming = rest, seen = model.seen ++ [ event ] }, attempt event )


attempt : Event -> Cmd Msg
attempt event =
    case event of
        Section _ ->
            Task.perform identity (Task.succeed Step)

        Run _ thunk ->
            let
                toMsg =
                    thunk
                        >> Maybe.map Fail
                        >> Maybe.withDefault Step
            in
            Process.sleep 0
                |> Task.perform toMsg



-- VIEW


view : Model -> Html msg
view model =
    div
        [ style "max-width" "960px"
        , style "font-family" fonts
        , style "margin" "0 auto"
        ]
        [ viewHeader (floatLength model.upcoming) (floatLength model.seen)
        , viewRunner model.final model.seen
        ]


viewRunner : Final -> List Event -> Html msg
viewRunner final seen =
    pre
        [ style "border-radius" "1px"
        , style "line-height" "1.75em"
        , style "padding" "3em"
        ]
        (terminalText noBreak seen ++ viewFinal final)


viewHeader : Float -> Float -> Html msg
viewHeader numRemaining numSeen =
    div
        [ style "display" "flex"
        , style "flex-direction" "row"
        , style "justify-content" "space-between"
        , style "align-items" "center"
        ]
        [ h1
            []
            [ text "Fill in the Blanks" ]
        , progress
            [ value (String.fromFloat numSeen)
            , Html.Attributes.max (String.fromFloat (numSeen + numRemaining))
            ]
            []
        ]


viewFinal : Final -> List (Html msg)
viewFinal final =
    case final of
        InProgress ->
            [ node "cursor"
                [ style "animation" "1s blink ease infinite"
                , style "background-color" "black"
                , title "Fill in the next blank to continue."
                ]
                [ text " "
                ]
            ]

        Finished ->
            [ text "🎉"
            , b
                [ style "color" "#2AA198" ]
                [ text "\n\nCONGRATULATIONS - You're all done!"
                ]
            ]

        Failed { given, description, reason } ->
            let
                failureText =
                    case given of
                        Nothing ->
                            format description reason

                        Just x ->
                            "GIVEN: " ++ x ++ "\n" ++ format description reason
            in
            [ b
                [ class "failed"
                , style "color" "#D5200C"
                ]
                [ text "✗\n\n"
                , text failureText
                ]
            ]


terminalText : String -> List Event -> List (Html msg)
terminalText sectionBreak events =
    case events of
        [] ->
            []

        (Section description) :: rest ->
            b
                []
                [ text
                    (sectionBreak
                        ++ "-- "
                        ++ String.toUpper description
                        ++ "\n"
                    )
                ]
                :: terminalText largeBreak rest

        (Run description _) :: [] ->
            b
                []
                [ text ("\n" ++ description ++ " ")
                ]
                :: []

        (Run description _) :: rest ->
            span [ class "passed" ]
                [ text ("\n" ++ description ++ " ✔")
                ]
                :: terminalText largeBreak rest


fonts : String
fonts =
    String.join ","
        [ "'Source Sans Pro'"
        , "'Trebuchet MS'"
        , "'Lucida Grande'"
        , "'Bitstream Vera Sans'"
        , "'Helvetica Neue'"
        , "sans-serif"
        ]


noBreak : String
noBreak =
    ""


largeBreak : String
largeBreak =
    "\n\n\n"


floatLength : List a -> Float
floatLength =
    toFloat << List.length
