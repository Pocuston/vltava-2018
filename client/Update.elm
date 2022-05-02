module Update exposing (..)

import Model exposing (..)
import Utils exposing (..)
import Server
import Task exposing (Task)
import List exposing (..)
import Date
import Time exposing (Time, second)
import Maybe exposing (andThen)
import Debug exposing (log)
import Dict exposing (Dict)
import Http
import Toasty
import Toasty.Defaults
import Debug


init : ( Model, Cmd Msg )
init =
    let
        emptySections =
            []
    in
        ( { sections = emptySections
          , startTime = Nothing
          , currentSection = Nothing
          , currentSectionTime = Nothing
          , sectionEditForm = emptySectionEditForm
          , nextSection = Nothing
          , remainingTime = Nothing
          , targetTime = getTargetTime emptySections
          , diffTime = getDiffComparedToPlan emptySections
          , scalars = Dict.empty
          , toasties = Toasty.initialState
          , lastUpdate = 0
          , isResetModalOpen = False
          }
        , Server.getSections
        )


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        receNotYetStarted =
            case model.startTime of
                Nothing ->
                    True

                Just _ ->
                    False

        receIsOver =
            case model.currentSection of
                Nothing ->
                    True

                Just _ ->
                    False
    in
        if receNotYetStarted || receIsOver then
            Sub.none
        else
            Time.every second Tick


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Start ->
            ( model, Task.perform ReceiveStartTime Time.now )

        ReceiveStartTime time ->
            let
                currentSection =
                    head model.sections

                currentSectionScalarId =
                    getCurrentSectionScalarId model.scalars

                startTimeScalarId =
                    getStartTimeScalarId model.scalars
            in
                case currentSection of
                    Just firstSection ->
                        ( model, Server.start time firstSection startTimeScalarId currentSectionScalarId )

                    Nothing ->
                        ( model, Cmd.none )
                            |> addToast (Toasty.Defaults.Error "Nejde spustit závod, protože nemáš v pořádku načtená data ze serveru" "Načti je tlačítkem 'Aktualizovat'")

        Changeover section ->
            let
                sectionEditForm =
                    model.sectionEditForm

                --there may be real time already saved for current section, so if form value is empty, we use saved value
                realTime =
                    if String.isEmpty sectionEditForm.realTime then
                        maybeTimeToString section.realTime
                    else
                        sectionEditForm.realTime

                validationResult =
                    validateAndUpdateSection section { sectionEditForm | realTime = realTime }

                currentSection =
                    findNextSection section model.sections

                --when there is no current section in progress, the race is over and remaining time is hidden
                remainingTime =
                    case currentSection of
                        Nothing ->
                            Nothing

                        Just _ ->
                            model.remainingTime

                currentSectionScalarId =
                    getCurrentSectionScalarId model.scalars
            in
                case validationResult of
                    Ok updatedSection ->
                        ( { model | sectionEditForm = emptySectionEditForm }
                        , Server.changeover updatedSection currentSection currentSectionScalarId
                        )

                    Err updatedForm ->
                        ( { model | sectionEditForm = updatedForm }, Cmd.none )
                            |> addToast (Toasty.Defaults.Error "Neplatný formát času" "Čas musí být ve formátu 'mmss', 'mm:ss', nebo 'hh:mm:ss'")

        StartSectionFormEdit section ->
            let
                --If edit form already exists and belongs to the same section, we use it so the edited values are not reset
                editForm =
                    if model.sectionEditForm.sectionId == section.id then
                        model.sectionEditForm
                    else
                        (SectionEditForm section.id (timeToString section.plannedTime) (maybeTimeToString section.realTime) True)
            in
                ( { model | sectionEditForm = editForm }, Cmd.none )

        UpdateSectionFormItem sectionFormItem value ->
            let
                currentForm =
                    model.sectionEditForm

                updatedForm =
                    case sectionFormItem of
                        PlannedTime ->
                            { currentForm | plannedTime = value }

                        RealTime ->
                            { currentForm | realTime = value }

                        Readonly ->
                            currentForm
            in
                ( { model | sectionEditForm = updatedForm }, Cmd.none )

        ValidateAndSaveEditForm section ->
            let
                validationResult =
                    validateAndUpdateSection section model.sectionEditForm
            in
                case validationResult of
                    Ok updatedSection ->
                        ( { model
                            | sectionEditForm = emptySectionEditForm
                          }
                        , Server.updateSection updatedSection
                        )

                    Err updatedForm ->
                        ( { model | sectionEditForm = updatedForm }, Cmd.none )
                            |> addToast (Toasty.Defaults.Error "Neplatný formát času" "Čas musí být ve formátu 'mmss', 'mm:ss', nebo 'hh:mm:ss'")

        Tick time ->
            case model.startTime of
                Nothing ->
                    ( model, Cmd.none )

                Just startTime ->
                    let
                        currentSection =
                            model.currentSection
                                |> andThen (getSection model.sections)

                        currentSectionTime =
                            currentSection
                                |> Maybe.andThen .timeToFinish
                                |> Maybe.andThen (\t -> Just (t - time))

                        remainingTime =
                            case currentSection of
                                Nothing ->
                                    Nothing

                                Just _ ->
                                    Just (getRemainingTime model.sections time)

                        isTimeToReload =
                            if (time - model.lastUpdate) > 30 * second then
                                True
                            else
                                False
                    in
                        ( { model
                            | currentSectionTime = currentSectionTime
                            , remainingTime = remainingTime
                            , lastUpdate =
                                if isTimeToReload then
                                    time
                                else
                                    model.lastUpdate
                          }
                        , if isTimeToReload then
                            Server.getSections
                          else
                            Cmd.none
                        )

        UpdateFromServer data ->
            case (data |> log "data") of
                Ok data ->
                    let
                        scalars =
                            getScalarsDict data

                        startTime =
                            scalars
                                |> Dict.get "startTime"
                                |> Maybe.andThen
                                    (\s ->
                                        s.value
                                            |> Maybe.withDefault ""
                                            |> String.toFloat
                                            |> Result.toMaybe
                                    )

                        sections =
                            data.sections
                                |> List.sortBy (\section -> section.order)
                                |> updateTimeToStart startTime

                        currentSectionId =
                            scalars
                                |> Dict.get "currentSection"
                                |> Maybe.andThen
                                    (\s ->
                                        s.value
                                    )

                        currentSection =
                            currentSectionId
                                |> Maybe.andThen (\sectionId -> sectionById sectionId sections)

                        nextSection =
                            currentSection
                                |> andThen (\section -> findNextSection section sections)

                        --when there is no current section in progress, the race is over and remaining time is hidden
                        remainingTime =
                            case currentSection of
                                Nothing ->
                                    Nothing

                                Just _ ->
                                    model.remainingTime
                    in
                        ( { model
                            | sections = sections
                            , startTime = startTime
                            , currentSection = currentSection
                            , nextSection = nextSection
                            , remainingTime = remainingTime
                            , targetTime = getTargetTime sections
                            , diffTime = getDiffComparedToPlan sections
                            , scalars = scalars
                          }
                        , Cmd.none
                        )

                Err msg ->
                    ( model, Cmd.none )
                        |> addToast (Toasty.Defaults.Error "Nepodařilo se načíst data." "Možná jsi mimo signál. Zkus tlačítko 'Aktualizovat' později")

        SectionUpdate result ->
            case result of
                Result.Err msg ->
                    let
                        m =
                            msg |> log "Error"
                    in
                        ( model, Cmd.none )
                            |> addToast (Toasty.Defaults.Error "Nepodařilo se uložit změnu." "Možná jsi mimo signál. Zkus to později")

                _ ->
                    ( model, Server.getSections )
                        |> addToast (Toasty.Defaults.Success "Změna byla v pořádku uložena." "")

        Reload ->
            ( model, Server.getSections )

        ToastyMsg subMsg ->
            Toasty.update Toasty.config ToastyMsg subMsg model

        OpenResetDialog ->
            ( { model | isResetModalOpen = True }, Cmd.none )

        CloseResetDialog ->
            ( { model | isResetModalOpen = False }, Cmd.none )

        Reset ->
            let
                currentSectionScalarId =
                    getCurrentSectionScalarId model.scalars

                startTimeScalarId =
                    getStartTimeScalarId model.scalars
            in
                ( { model | isResetModalOpen = False }, Server.reset model.sections startTimeScalarId currentSectionScalarId )
