module Tests exposing (..)

import Model exposing (Section)
import Utils exposing (..)
import Expect exposing (..)
import List exposing (map)
import Test exposing (..)
import Date exposing (fromString, toTime, fromTime)
import Result exposing (map2)
import Debug exposing (log, crash)
import Maybe exposing (withDefault)
import Time exposing (minute, second, hour)


testSections =
    [ Section "1" 1 "1." "Kuba" (plannedTimeFromString "40:00") (realTimeFromString "40:00") Nothing Nothing
    , Section "2" 2 "2." "Honza" (plannedTimeFromString "45:00") (realTimeFromString "45:00") Nothing Nothing
    , Section "3" 3 "3" "Zdenda" (plannedTimeFromString "50:00") (realTimeFromString "50:00") Nothing Nothing
    , Section "4" 4 "4" "Tomas" (plannedTimeFromString "55:00") (realTimeFromString "55:00") Nothing (Result.toMaybe (timeFromString "1:00:00"))
    ]


suite : Test
suite =
    describe "Vltava app test suite"
        [ describe "Utils.timeToString tests"
            [ test "correctly formats one minute" <|
                \() ->
                    minute
                        |> timeToString
                        |> Expect.equal "01:00"
            , test "correctly formats minutes and seconds" <|
                \() ->
                    (40 * minute)
                        + (38 * Time.second)
                        |> timeToString
                        |> Expect.equal "40:38"
            , test "correctly formats hours" <|
                \() ->
                    (2 * hour)
                        + (9 * minute)
                        + 16
                        * second
                        |> timeToString
                        |> Expect.equal "02:09:16"
            ]
        , describe "Utils.timeFromString tests"
            [ test "correctly parses '01:00'" <|
                \() ->
                    timeFromString "01:00"
                        |> Expect.equal (Ok minute)
            , test "correctly parses '1:00'" <|
                \() ->
                    timeFromString "1:00"
                        |> Expect.equal (Ok minute)
            , test "correctly parses '0100'" <|
                \() ->
                    timeFromString "0100"
                        |> Expect.equal (Ok minute)
            , test "correctly parses '04:38'" <|
                \() ->
                    timeFromString "40:38"
                        |> Expect.equal (Ok ((40 * minute) + (38 * Time.second)))
            , test "correctly parses '4038'" <|
                \() ->
                    timeFromString "4038"
                        |> Expect.equal (Ok ((40 * minute) + (38 * Time.second)))
            , test "correctly parses '02:09:16'" <|
                \() ->
                    timeFromString "02:09:16"
                        |> Expect.equal
                            (Ok
                                ((2 * hour)
                                    + (9 * minute)
                                    + 16
                                    * second
                                )
                            )
            , test "Fails on invalid string 'xx:09:16'" <|
                \() ->
                    timeFromString "xx:09:16"
                        |> Expect.equal (Err ("Invalid time format"))
            , test "Fails on empty string ''" <|
                \() ->
                    timeFromString ""
                        |> Expect.equal (Err ("Invalid time format"))
            , test "Fails on invalid string '1:09:'" <|
                \() ->
                    timeFromString "xx:09:16"
                        |> Expect.equal (Err ("Invalid time format"))
            , test "Fails on invalid string '02:72:16'" <|
                \() ->
                    timeFromString "02:72:16"
                        |> Expect.equal (Err ("Invalid time format"))
            ]
        , describe "Utils.timeDiffInSeconds tests"
            [ test "computes elapsed seconds" <|
                \_ ->
                    let
                        dateA =
                            fromString "2018-04-24T19:45:30+02:00"

                        dateB =
                            fromString "2018-04-24T19:40:00+02:00"

                        result =
                            map2 (,) dateA dateB

                        fiveThirty =
                            (5 * 60) + 30
                    in
                        case result of
                            Ok values ->
                                (timeDiffInSeconds (Tuple.first values) (Tuple.second values))
                                    |> Expect.equal fiveThirty

                            Err msg ->
                                Expect.fail "!!!"
            ]
        , describe "Utils.updateTimeToStart tests"
            [ test "when time from start is 0" <|
                \_ ->
                    let
                        sections =
                            (updateTimeToStart
                                (Just
                                    0
                                )
                                testSections
                            )

                        updatedTimes =
                            sections |> map (\s -> ( s.timeToStart |> withDefault 0, s.timeToFinish |> withDefault 0 ))

                        expectedTimes =
                            [ ( 0 * minute, 40 * minute )
                            , ( 40 * minute, minute * (40 + 45) )
                            , ( minute * (40 + 45), minute * (40 + 45 + 50) )
                            , ( minute * (40 + 45 + 50), minute * (40 + 45 + 50 + 55) )
                            ]
                    in
                        updatedTimes
                            |> Expect.equal expectedTimes
            , test "when time from start is 5 minutes, time to start is updated" <|
                \_ ->
                    let
                        sections =
                            (updateTimeToStart
                                (Just (-5 * minute))
                                testSections
                            )

                        updatedTimes =
                            sections
                                |> map (\s -> s.timeToStart |> withDefault 0)

                        expectedTimes =
                            [ minute * -5
                            , minute * (40 - 5)
                            , minute * (40 + 45 - 5)
                            , minute * (40 + 45 + 50 - 5)
                            ]
                    in
                        updatedTimes
                            |> Expect.equal expectedTimes
            , test "getRemainingTime gives sum of planned times when not started yet" <|
                \_ ->
                    let
                        result =
                            getRemainingTime testSections 0
                    in
                        result
                            |> Expect.equal (hour)
            , test "getRemainingTime gives updated time 5 minutes after start" <|
                \_ ->
                    let
                        result =
                            getRemainingTime testSections (5 * minute)
                    in
                        result
                            |> Expect.equal (55 * minute)
            , test "target time gives sum of estimated times" <|
                \_ ->
                    let
                        result =
                            getTargetTime testSections
                    in
                        result
                            |> Expect.equal
                                ((40 + 45 + 50 + 55)
                                    * minute
                                )
            ]
        , describe "Auxiliary tests to check how Date and Time works"
            [ test "Date fromString -> toTime -> fromTime" <|
                \_ ->
                    let
                        date =
                            fromString "2018-04-24T19:40:00+02:00"

                        time =
                            case date of
                                Ok date ->
                                    toTime date

                                Err _ ->
                                    0

                        dateFromTime =
                            fromTime time
                    in
                        Expect.equal
                            (fromString "2018-04-24T19:40:00+02:00")
                            (Ok dateFromTime)
            , test "Add milliseconds to Time" <|
                \_ ->
                    let
                        date =
                            fromString "2018-04-24T19:40:00+02:00"

                        time =
                            case date of
                                Ok date ->
                                    toTime date

                                Err _ ->
                                    0

                        plus5Seconds =
                            time
                                + (5 * 1000)
                                |> fromTime
                    in
                        Expect.equal
                            (fromString "2018-04-24T19:40:05+02:00")
                            (Ok plus5Seconds)
            , test "Compute time diff between two dates" <|
                \_ ->
                    let
                        dateA =
                            fromString "2018-04-24T19:40:00+02:00"

                        dateB =
                            fromString "2018-04-24T19:45:00+02:00"

                        timeDiff =
                            (Result.map2
                                (\a b -> (toTime b) - (toTime a))
                                dateA
                                dateB
                            )

                        fiveMinutes =
                            5 * 60 * 1000
                    in
                        case timeDiff of
                            Err _ ->
                                Expect.fail "!!!"

                            Ok timeDiff ->
                                Expect.equal timeDiff fiveMinutes
            ]
        ]
