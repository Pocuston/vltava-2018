module Main exposing (..)

import Utils exposing (..)
import Model exposing (..)
import Update exposing (..)
import List exposing (..)
import Time exposing (Time)
import Date
import Maybe exposing (withDefault, andThen)
import Html exposing (Html, button, div, text, img, span)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onFocus)
import Bulma.CDN exposing (..)
import Bulma.Modifiers exposing (..)
import Bulma.Elements exposing (..)
import Bulma.Components exposing (..)
import Bulma.Layout exposing (hero, heroModifiers, heroHead, container)
import Bulma.Form exposing (..)
import Toasty
import Toasty.Defaults


main : Platform.Program Basics.Never Model Msg
main =
    Html.program
        { view = view
        , update = update
        , init = init
        , subscriptions = Update.subscriptions
        }



-- VIEW


view : Model -> Html Msg
view model =
    let
        startButtonVisible =
            case model.startTime of
                Nothing ->
                    True

                Just date ->
                    False
    in
        div []
            [ stylesheet
            , menu startButtonVisible
            , hero { heroModifiers | color = Warning }
                [ style [ ( "padding", "12px" ) ] ]
                [ span []
                    []
                , heroHead
                    [ style [] ]
                    [ div []
                        [ currentSectionElement model.currentSection model.currentSectionTime
                        , nextSectionElement model.nextSection
                        ]
                    , div []
                        [ remainingTimeElement model.remainingTime
                        , targetTimeElement model.targetTime
                        , diffTimeElement model.diffTime
                        ]
                    ]
                ]
            , div [ class "container" ]
                [ table { tableModifiers | narrow = True, hoverable = True, fullWidth = True }
                    [ style [ ( "margin", "auto" ), ( "margin-top", "24px" ) ] ]
                    [ tableHead []
                        [ tableRow False
                            []
                            [ tableCellHead []
                                [ text "Úsek" ]
                            , tableCellHead [] [ text "" ]
                            , tableCellHead [] [ text "" ]
                            , tableCellHead [] [ text "" ]
                            , tableCellHead [] [ text "Borec" ]
                            , tableCellHead [ style [ ( "text-align", "right" ), ( "min-width", "84px" ) ] ] [ text "Start" ]
                            , tableCellHead [ style [ ( "text-align", "right" ), ( "min-width", "84px" ) ] ] [ text "Plán" ]
                            , tableCellHead [ style [ ( "text-align", "right" ), ( "min-width", "84px" ) ] ] [ text "Skutečný čas" ]
                            , tableCellHead [] [ text "" ]
                            ]
                        ]
                    , tableBody [] (List.map (sectionTableRow model.currentSection model.sectionEditForm model.startTime) model.sections)
                    ]
                ]
            , div [ class "container", style [ ( "text-align", "left" ) ] ]
                [ Bulma.Elements.button { buttonModifiers | color = Default } [ style [ ( "margin", "8px" ) ], onClick OpenResetDialog ] [ text "Reset" ]
                ]
            , Toasty.view Toasty.Defaults.config Toasty.Defaults.view ToastyMsg model.toasties
            , resetModal model.isResetModalOpen
            ]


menu : Bool -> Navbar Msg
menu startButtonVisible =
    navbar navbarModifiers
        []
        [ navbarBrand []
            (navbarBurger False [] [ navbarItem True [] [] ])
            [ navbarItem False
                []
                [ img [ src "./logo_gomango.jpg" ] []
                ]
            ]
        , navbarMenu True
            []
            [ navbarEnd []
                [ if startButtonVisible then
                    navbarItem True
                        []
                        [ Bulma.Elements.button { buttonModifiers | color = Success }
                            [ onClick Start ]
                            [ Html.i [ class "fa fa-play-circle" ] [], text " Start!" ]
                        ]
                  else
                    nothing
                , navbarItem True
                    []
                    [ Bulma.Elements.button { buttonModifiers | color = Light } [ onClick Reload ] [ Html.i [ class "fa fa-sync" ] [], text " Aktualizovat" ]
                    ]
                ]
            ]
        ]


buttonsColumnWidths : Devices (Maybe Width)
buttonsColumnWidths =
    { mobile = Just Width9
    , tablet = Just Width5
    , desktop = Nothing
    , widescreen = Nothing
    , fullHD = Nothing
    }


widths : Maybe Width -> Devices (Maybe Width)
widths width =
    { mobile = width
    , tablet = width
    , desktop = width
    , widescreen = width
    , fullHD = width
    }


commonTableCellStyles : List ( String, String )
commonTableCellStyles =
    [ ( "vertical-align", "middle" )
    ]


higlightedTableCellStyles : List ( String, String )
higlightedTableCellStyles =
    commonTableCellStyles ++ [ ( "font-weight", "bold" ) ]


finishedTableCellStyles : List ( String, String )
finishedTableCellStyles =
    commonTableCellStyles ++ [ ( "color", "Silver" ) ]


iconLinkStyle : List ( String, String )
iconLinkStyle =
    [ ( "color", "STEELBLUE" ) ]


currentSectionElement : Maybe Section -> Maybe Time -> Html msg
currentSectionElement currentSection currentSectionTime =
    let
        timeToFinish =
            currentSection
                |> andThen (\s -> s.timeToFinish)
                |> withDefault 0
    in
        case currentSection of
            Just currentSection ->
                span []
                    [ text "Právě běží "
                    , infoTag currentSection.runner
                    , text " úsek "
                    , infoTag currentSection.name
                    , text " a bude v cíli za "
                    , infoTag
                        (currentSectionTime
                            |> withDefault 0
                            |> timeToString
                        )
                    , text ". "
                    ]

            Nothing ->
                nothing


nextSectionElement : Maybe Section -> Html msg
nextSectionElement nextSection =
    case nextSection of
        Just nextSection ->
            span []
                [ text "Na další úsek se chystá "
                , infoTag nextSection.runner
                , text ". "
                ]

        Nothing ->
            nothing


remainingTimeElement : Maybe Time -> Html msg
remainingTimeElement time =
    case time of
        Just time ->
            span []
                [ text "Do cíle zbývá "
                , infoTag (timeToString time)
                , text ". "
                ]

        Nothing ->
            nothing


targetTimeElement : Time -> Html msg
targetTimeElement time =
    span []
        [ text "Cílový čas bude "
        , infoTag (timeToString time)
        , text ". "
        ]


diffTimeElement : Time -> Html msg
diffTimeElement time =
    span []
        [ text "Rozdíl oproti plánu je "
        , infoTag (timeToString time)
        , text "."
        ]


sectionTableRow : Maybe Section -> SectionEditForm -> Maybe Time -> Section -> Html Msg
sectionTableRow currentSection sectionEditForm raceStartTime section =
    let
        isCurrentRow =
            case currentSection of
                Nothing ->
                    False

                Just value ->
                    section.id == value.id

        isEdited =
            sectionEditForm.sectionId == section.id

        isFinished =
            case currentSection of
                Nothing ->
                    False

                Just currentSection ->
                    currentSection.order > section.order

        -- we pass sectionEditForm to cells only it the row is currently being edited
        editForm =
            if isEdited then
                Just sectionEditForm
            else
                Nothing

        cellStyle =
            if isCurrentRow then
                higlightedTableCellStyles
            else if isFinished then
                finishedTableCellStyles
            else
                commonTableCellStyles

        startTime =
            sectionStartTimeToString section.timeToStart

        isBigChangeOver =
            rem section.order 6 == 0 && section.order /= 36
    in
        tableRow isCurrentRow
            [ style
                (if isBigChangeOver then
                    [ ( "border-bottom", "solid 4px Gold  " ) ]
                 else
                    []
                )
            ]
            [ tableCell [ style cellStyle ] [ text section.name ]
            , tableCell [ style cellStyle ] [ Html.a [ href section.runnerRoute, target "_blank", style iconLinkStyle ] [ Html.i [ class "fa fa-map-marker-alt" ] [] ] ]
            , tableCell [ style cellStyle ] [ Html.a [ href section.carRoute, target "_blank", style iconLinkStyle ] [ Html.i [ class "fa fa-car" ] [] ] ]
            , tableCell [ style cellStyle ] [ Html.a [ href section.proposition, target "_blank", style iconLinkStyle ] [ Html.i [ class "fa fa-info-circle" ] [] ] ]
            , tableCell [ style cellStyle ] [ text section.runner ]
            , tableCell [ style (cellStyle ++ [ ( "text-align", "right" ) ]) ] [ text startTime ]
            , timeTableCell (Just section.plannedTime) section Readonly editForm
            , timeTableCell section.realTime section RealTime editForm
            , tableCell [ style commonTableCellStyles ]
                [ if isCurrentRow then
                    (Bulma.Elements.button { buttonModifiers | color = Warning, size = Small } [ onClick (Changeover section) ] [ Html.i [ class "fa fa-check" ] [], text " Předávka" ])
                  else
                    nothing
                , if isEdited && not isCurrentRow then
                    (Bulma.Elements.button { buttonModifiers | color = Primary, size = Small } [ onClick (ValidateAndSaveEditForm section) ] [ Html.i [ class "fa fa-check" ] [], text " Uložit" ])
                  else
                    nothing
                ]
            ]


timeTableCell : Maybe Time -> Section -> SectionFormItem -> Maybe SectionEditForm -> Html Msg
timeTableCell time section sectionFormItem sectionEditForm =
    let
        isEditable =
            case sectionFormItem of
                Readonly ->
                    False

                _ ->
                    True

        displayValue =
            case sectionEditForm of
                Just editForm ->
                    case sectionFormItem of
                        RealTime ->
                            editForm.realTime

                        PlannedTime ->
                            editForm.plannedTime

                        Readonly ->
                            maybeTimeToString time

                Nothing ->
                    maybeTimeToString time

        inputAttributes =
            [ value displayValue
            , placeholder "mmss"
            , style
                [ ( "text-align", "right" ), ( "width", "78px" ) ]
            , onFocus (StartSectionFormEdit section)
            , onInput (UpdateSectionFormItem sectionFormItem)
            ]

        inputColor =
            case sectionEditForm of
                Just editForm ->
                    if editForm.isRealTimeValid then
                        Default
                    else
                        Danger

                Nothing ->
                    Default
    in
        case isEditable of
            True ->
                tableCell [ style commonTableCellStyles ]
                    [ controlInput { controlInputModifiers | size = Standard, color = inputColor } [ style [ ( "text-align", "right" ) ] ] inputAttributes []
                    ]

            False ->
                tableCell [ style (commonTableCellStyles ++ [ ( "text-align", "right" ) ]) ] [ text displayValue ]


nothing : Html msg
nothing =
    text ""


infoTag : String -> Html msg
infoTag msg =
    tag { tagModifiers | size = Medium, color = Light } [ style [ ( "font-weight", "bold" ) ] ] [ text msg ]


resetModal : Bool -> Html Msg
resetModal isOpened =
    modal isOpened
        []
        [ modalBackground [] []
        , modalContent []
            [ message { messageModifiers | color = Warning }
                []
                [ messageHeader [] [ text "Potvrzení" ]
                , messageBody []
                    [ div [ style [ ( "padding", "12px" ) ] ] [ text "Opravdu chceš resetovat všechny zaznamenané časy od startu?" ]
                    , div [ style [ ( "text-align", "center" ) ] ]
                        [ (Bulma.Elements.button { buttonModifiers | color = Danger }
                            [ onClick Reset ]
                            [ Html.i [ class "fa fa-check" ] [], text " Jo, chci!" ]
                          )
                        , (Bulma.Elements.button
                            { buttonModifiers | color = Success }
                            [ style [ ( "margin-left", "8px" ) ], onClick CloseResetDialog ]
                            [ Html.i [ class "fa fa-ban" ] [], text " Radši ne" ]
                          )
                        ]
                    ]
                ]
            ]
        , modalClose Large [ onClick CloseResetDialog ] []
        ]
