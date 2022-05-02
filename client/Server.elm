module Server exposing (getSections, updateSection, changeover, start, reset)

import Model exposing (..)
import Http exposing (stringBody, send)
import Task exposing (Task)
import Debug exposing (log)
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import String
import String.Extra
import Time exposing (Time)


apiServer : String
apiServer =
    "/vltava/api"


getSections : Cmd Msg
getSections =
    let
        body =
            toStringBody
                """
                {
                    "query":"query
                    {
                        allSections {
                            id,
                            order,
                            name,
                            carRoute,
                            runnerRoute,
                            proposition,
                            runner,
                            plannedTime,
                            realTime },
                        allScalarses {
                            id,
                            name,
                            value}
                        }"
                }"""
    in
        Http.send UpdateFromServer <| Http.post apiServer body dataFromServerDecoder


updateSection : Section -> Cmd Msg
updateSection section =
    let
        realTime =
            case section.realTime of
                Just realTime ->
                    toString realTime

                Nothing ->
                    "null"

        body =
            toStringBody
                ("""
                {
                    "query":"mutation
                    {
                        updateSection(
                            id: \\\""""
                    ++ section.id
                    ++ """\\"
                            realTime: """
                    ++ realTime
                    ++ """
                        ) { id  }
                    }"
                }"""
                )
    in
        Http.send SectionUpdate <| Http.post apiServer body (Decode.at [ "data", "updateSection", "id" ] Decode.string)


changeover : Section -> Maybe Section -> String -> Cmd Msg
changeover previousSection currentSection currentSectionScalarId =
    let
        realTime =
            case previousSection.realTime of
                Just realTime ->
                    toString realTime

                Nothing ->
                    "null"

        currentSectionId =
            case currentSection of
                Nothing ->
                    "null"

                Just currentSection ->
                    "\\\"" ++ currentSection.id ++ "\\\""

        body =
            toStringBody
                ("""
                {
                    "query":"mutation
                    {
                        updateSection: updateSection(
                            id: \\\"""" ++ previousSection.id ++ """\\"
                            realTime: """ ++ realTime ++ """
                        ) {
                            id
                        },
                        updateScalars: updateScalars(
                            id: \\\"""" ++ currentSectionScalarId ++ """\\"
                            value: """ ++ currentSectionId ++ """
                        ) {
                        id
                    }
                    }"
                }""")
    in
        Http.send SectionUpdate <| Http.post apiServer body (Decode.at [ "data", "updateSection", "id" ] Decode.string)


start : Time -> Section -> String -> String -> Cmd Msg
start startTime firstSection startTimeScalarId currentSectionScalarId =
    let
        body =
            toStringBody
                ("""
                {
                    "query":"mutation
                    {
                        updateStartTime: updateScalars(
                            id: \\\"""" ++ startTimeScalarId ++ """\\"
                            value: \\\"""" ++ (toString startTime) ++ """\\"
                        ) {
                            id
                        },
                        updateCurrentSection: updateScalars(
                            id: \\\"""" ++ currentSectionScalarId ++ """\\"
                            value: \\\"""" ++ firstSection.id ++ """\\"
                        ) {
                            id
                        }
                    }"
                }""")
    in
        Http.send SectionUpdate <| Http.post apiServer body (Decode.at [ "data", "updateStartTime", "id" ] Decode.string)


reset : List Section -> String -> String -> Cmd Msg
reset sections startTimeScalarId currentSectionScalarId =
    let
        indexes =
            List.range 1 (List.length sections)

        sectionQueries =
            sections
                |> List.map2 sectionResetQuery indexes
                |> String.join " "

        body =
            toStringBody
                ("""
                {
                    "query":"mutation
                    { """
                    ++ sectionQueries
                    ++ """
                        updateStartTime: updateScalars(
                            id: \\\""""
                    ++ startTimeScalarId
                    ++ """\\"
                            value: null
                        ) {
                            id
                        },
                        updateCurrentSection: updateScalars(
                            id: \\\""""
                    ++ currentSectionScalarId
                    ++ """\\"
                            value: null
                        ) {
                            id
                        }
                        }"
                    }"""
                )
    in
        Http.send SectionUpdate <| Http.post apiServer body (Decode.at [ "data", "updateStartTime", "id" ] Decode.string)


sectionResetQuery : Int -> Section -> String
sectionResetQuery index section =
    """
    reset""" ++ toString index ++ """: updateSection(id: \\\"""" ++ section.id ++ """\\"
                            realTime: null
                        ) {
                            id
                        }
    """


dataFromServerDecoder : Decode.Decoder RawDataFromServer
dataFromServerDecoder =
    Pipeline.decode RawDataFromServer
        |> Pipeline.requiredAt [ "data", "allSections" ] (Decode.list sectionDecoder)
        |> Pipeline.requiredAt [ "data", "allScalarses" ] (Decode.list scalarDecoder)


sectionDecoder : Decode.Decoder Section
sectionDecoder =
    Pipeline.decode Section
        |> Pipeline.required "id" Decode.string
        |> Pipeline.required "order" Decode.int
        |> Pipeline.required "name" Decode.string
        |> Pipeline.required "runner" Decode.string
        |> Pipeline.required "carRoute" Decode.string
        |> Pipeline.required "runnerRoute" Decode.string
        |> Pipeline.required "proposition" Decode.string
        |> Pipeline.required "plannedTime" Decode.float
        |> Pipeline.optional "realTime" (Decode.map Just Decode.float) Nothing
        |> Pipeline.hardcoded Nothing
        |> Pipeline.hardcoded Nothing


scalarDecoder : Decode.Decoder Scalar
scalarDecoder =
    Pipeline.decode Scalar
        |> Pipeline.required "id" Decode.string
        |> Pipeline.required "name" Decode.string
        |> Pipeline.optional "value" (Decode.map Just Decode.string) Nothing


toStringBody : String -> Http.Body
toStringBody string =
    Http.stringBody "application/json"
        (string
            |> String.Extra.replace "\n" ""
        )
