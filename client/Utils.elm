module Utils exposing (..)

import Model exposing (..)
import String exposing (padLeft)
import Array exposing (Array, fromList, length, get)
import Date exposing (Date, toTime)
import Time exposing (Time, inMilliseconds, hour, minute, second)
import Result exposing (fromMaybe, toMaybe)
import List exposing (foldr, head, tail, map)
import Maybe exposing (withDefault)
import Debug exposing (log)
import Dict exposing (Dict)
import Toasty
import Toasty.Defaults


errorMessage : String
errorMessage =
    "Invalid time format"


parseTimeUnit : Maybe String -> Result String Float
parseTimeUnit value =
    value
        |> fromMaybe errorMessage
        |> Result.andThen String.toFloat


timeFromString : String -> Result String Time
timeFromString text =
    let
        values =
            text
                |> String.split ":"
                -- list of time units is reverted se we can count on fixed position of minutes and seconds wether there are hours presented
                |> List.reverse
                |> Array.fromList

        -- simple format is 'mmss' e.g. '4038' -> '00:40:38'
        isSimpleFormat =
            length values
                == 1
                && String.length text
                == 4

        seconds =
            (if not isSimpleFormat then
                get 0 values
                    |> parseTimeUnit
             else
                text
                    |> String.slice 2 4
                    |> String.toFloat
            )
                |> Result.andThen
                    (\seconds ->
                        if seconds >= 0 && seconds < 60 then
                            Ok seconds
                        else
                            Err errorMessage
                    )

        minutes =
            (if not isSimpleFormat then
                get 1 values
                    |> parseTimeUnit
             else
                text
                    |> String.slice 0 2
                    |> String.toFloat
            )
                |> Result.andThen
                    (\minutes ->
                        if minutes >= 0 && minutes < 60 then
                            Ok minutes
                        else
                            Err errorMessage
                    )

        hours =
            if not isSimpleFormat && length values == 3 then
                get 2 values
                    |> parseTimeUnit
                    |> Result.andThen
                        (\hours ->
                            if hours >= 0 then
                                Ok hours
                            else
                                Err errorMessage
                        )
            else
                Ok 0

        result =
            Result.map3 (\h m s -> { h = h, m = m, s = s }) hours minutes seconds
    in
        case result of
            Ok value ->
                Ok ((value.h * hour) + (value.m * minute) + (value.s * second))

            Err msg ->
                Err errorMessage


timeToString : Time -> String
timeToString time =
    let
        timeInseconds =
            time
                / 1000
                |> round
                |> abs

        hours =
            timeInseconds
                // 3600

        minutes =
            (rem timeInseconds 3600)
                // 60

        seconds =
            rem timeInseconds 60
    in
        (if time < 0 then
            "-"
         else
            ""
        )
            ++ (if hours > 0 then
                    padLeft 2 '0' (toString hours) ++ ":"
                else
                    ""
               )
            ++ padLeft 2 '0' (toString minutes)
            ++ ":"
            ++ padLeft 2 '0' (toString seconds)


maybeTimeToString : Maybe Time -> String
maybeTimeToString time =
    case time of
        Nothing ->
            ""

        Just value ->
            timeToString value


plannedTimeFromString : String -> Time
plannedTimeFromString text =
    case (timeFromString text) of
        Ok value ->
            value

        Err msg ->
            0


realTimeFromString : String -> Maybe Time
realTimeFromString text =
    timeFromString text
        |> toMaybe


updateSection : List Section -> Section -> List Section
updateSection sections section =
    let
        updateSection s =
            if s.id == section.id then
                section
            else
                s
    in
        sections |> List.map updateSection


validateAndUpdateSection : Section -> SectionEditForm -> Result SectionEditForm Section
validateAndUpdateSection section sectionEditForm =
    let
        --TODO use Result insted of Tuple
        realTimeValidation =
            case (timeFromString sectionEditForm.realTime) of
                Ok time ->
                    ( Just time, True )

                Err _ ->
                    ( section.realTime, False )

        updatedForm =
            { sectionEditForm
                | sectionId = section.id
                , isRealTimeValid = (Tuple.second realTimeValidation)
            }

        updatedSection =
            { section | realTime = (Tuple.first realTimeValidation) }
    in
        case Tuple.second realTimeValidation of
            True ->
                Ok updatedSection

            False ->
                Err updatedForm


emptySectionEditForm : SectionEditForm
emptySectionEditForm =
    SectionEditForm "" "" "" True


timeDiffInSeconds : Date -> Date -> Time
timeDiffInSeconds a b =
    let
        timeA =
            Date.toTime a

        timeB =
            Date.toTime b
    in
        (timeA - timeB) / 1000


estimatedTime : Section -> Time
estimatedTime section =
    section.realTime
        |> withDefault section.plannedTime


updateTimeToStart : Maybe Time -> List Section -> List Section
updateTimeToStart timeToStart sections =
    let
        section =
            (head sections)
                |> Maybe.andThen
                    (\section ->
                        case timeToStart of
                            Nothing ->
                                Just
                                    { section
                                        | timeToStart = Nothing
                                        , timeToFinish = Nothing
                                    }

                            Just timeToStart ->
                                Just
                                    { section
                                        | timeToStart = Just timeToStart
                                        , timeToFinish = Just (timeToStart + estimatedTime section)
                                    }
                    )

        remaining =
            tail sections
                |> withDefault []
    in
        case section of
            Nothing ->
                []

            Just section ->
                [ section ]
                    ++ updateTimeToStart section.timeToFinish remaining


sectionById : String -> List Section -> Maybe Section
sectionById sectionId sections =
    let
        s =
            head sections

        t =
            tail sections
                |> withDefault []
    in
        case s of
            Nothing ->
                Nothing

            Just section ->
                if section.id == sectionId then
                    Just section
                else
                    sectionById sectionId t


findNextSection : Section -> List Section -> Maybe Section
findNextSection section sections =
    let
        s =
            head sections

        t =
            tail sections
                |> withDefault []
    in
        case s of
            Nothing ->
                Nothing

            Just s ->
                if s.id == section.id then
                    head t
                else
                    findNextSection section t


getSection : List Section -> Section -> Maybe Section
getSection sections section =
    let
        s =
            head sections

        t =
            tail sections
                |> withDefault []
    in
        case s of
            Nothing ->
                Nothing

            Just s ->
                if s.id == section.id then
                    Just s
                else
                    getSection t section


getRemainingTime : List Section -> Time -> Time
getRemainingTime sections now =
    let
        lastSection =
            List.reverse sections
                |> List.head

        remaining =
            case lastSection of
                Nothing ->
                    0

                Just lastSection ->
                    lastSection.timeToFinish
                        |> Maybe.withDefault 0
    in
        remaining - now


getTargetTime : List Section -> Time
getTargetTime sections =
    foldr (+)
        0
        (sections
            |> map estimatedTime
        )


getDiffComparedToPlan : List Section -> Time
getDiffComparedToPlan sections =
    let
        targetTime =
            getTargetTime sections

        plannedTime =
            foldr (+)
                0
                (sections
                    |> map (\section -> section.plannedTime)
                )
    in
        targetTime - plannedTime


getScalarsDict : RawDataFromServer -> Dict String Scalar
getScalarsDict data =
    data.scalars
        |> map (\scalar -> ( scalar.name, scalar ))
        |> Dict.fromList


getCurrentSectionScalarId : Dict String Scalar -> String
getCurrentSectionScalarId scalars =
    scalars
        |> Dict.get "currentSection"
        |> Maybe.andThen
            (\s ->
                Just s.id
            )
        |> Maybe.withDefault "-1"


getStartTimeScalarId : Dict String Scalar -> String
getStartTimeScalarId scalars =
    scalars
        |> Dict.get "startTime"
        |> Maybe.andThen
            (\s ->
                Just s.id
            )
        |> Maybe.withDefault "-1"


addToast : Toasty.Defaults.Toast -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
addToast toast ( model, cmd ) =
    Toasty.addToast Toasty.Defaults.config ToastyMsg toast ( model, cmd )


sectionStartTimeToString : Maybe Time -> String
sectionStartTimeToString timeToStart =
    case timeToStart of
        Just timeToStart ->
            let
                dateValue =
                    Date.fromTime timeToStart
            in
                (Date.hour dateValue
                    |> toString
                    |> padLeft 2 ' '
                )
                    ++ ":"
                    ++ (Date.minute dateValue
                            |> toString
                            |> padLeft 2 '0'
                       )
                    ++ ":"
                    ++ (Date.second dateValue
                            |> toString
                            |> padLeft 2 '0'
                       )

        Nothing ->
            ""
