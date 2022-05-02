module Model exposing (..)

import Time exposing (Time)
import Date exposing (Date)
import Dict exposing (Dict)
import Http
import Toasty
import Toasty.Defaults


-- MODEL


type alias Model =
    { sections : List Section
    , startTime : Maybe Time
    , currentSection : Maybe Section
    , currentSectionTime : Maybe Time
    , nextSection : Maybe Section
    , sectionEditForm : SectionEditForm
    , remainingTime : Maybe Time
    , targetTime : Time
    , diffTime : Time
    , scalars : Dict String Scalar
    , toasties : Toasty.Stack Toasty.Defaults.Toast
    , lastUpdate : Time
    , isResetModalOpen : Bool
    }


type alias Section =
    { id : String
    , order : Int
    , name : String
    , runner : String
    , carRoute : String
    , runnerRoute: String
    , proposition: String
    , plannedTime : Time
    , realTime : Maybe Time
    , timeToStart : Maybe Time
    , timeToFinish : Maybe Time
    }


type alias SectionEditForm =
    { sectionId : String
    , plannedTime : String
    , realTime : String
    , isRealTimeValid : Bool
    }


type SectionFormItem
    = PlannedTime
    | RealTime
    | Readonly


type alias Scalar =
    { id : String
    , name : String
    , value : Maybe String
    }


type alias RawDataFromServer =
    { sections : List Section
    , scalars : List Scalar
    }


type Msg
    = Start
    | ReceiveStartTime Time.Time
    | Changeover Section
    | StartSectionFormEdit Section
    | UpdateSectionFormItem SectionFormItem String
    | ValidateAndSaveEditForm Section
    | Tick Time.Time
    | SectionUpdate (Result Http.Error String)
    | UpdateFromServer (Result Http.Error RawDataFromServer)
    | Reload
    | ToastyMsg (Toasty.Msg Toasty.Defaults.Toast)
    | OpenResetDialog
    | CloseResetDialog
    | Reset
