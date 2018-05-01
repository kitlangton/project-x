module Reminder exposing (..)

import Html exposing (Html, div, h1, h3, img, text, textarea, input)
import Html.Attributes exposing (class, classList, value, placeholder)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as JD
import Json.Encode as JE
import DatePicker
import Date


-- import Html.Attributes exposing (src)


type alias User =
    { name : String
    , uid : String
    , email : String
    }


type alias State =
    { reminder : Reminder
    , favorites : List User
    , visible : Bool
    , datePicker : DatePicker.DatePicker
    }


type alias Reminder =
    { remindText : String
    , remindDate : Maybe Date.Date
    , recipients : List User
    }


dateDecoder : JD.Decoder Date.Date
dateDecoder =
    JD.string
        |> JD.andThen
            (\dateString ->
                case (Date.fromString dateString) of
                    Ok date ->
                        JD.succeed date

                    Err errorString ->
                        JD.fail errorString
            )


encodeReminder : Reminder -> JE.Value
encodeReminder { remindText, remindDate, recipients } =
    JE.object
        [ ( "text", JE.string remindText ), ( "date", JE.string (toString remindDate) ), ( "recipients", JE.list <| List.map (JE.string << .uid) recipients ) ]


postReminder : Reminder -> Http.Request String
postReminder reminder =
    let
        reminderValue =
            encodeReminder reminder
    in
        Http.post "url" (Http.jsonBody reminderValue) JD.string


init : ( State, Cmd Msg )
init =
    let
        ( datePicker, datePickerCmd ) =
            DatePicker.init
    in
        { favorites = [], reminder = Reminder "" Nothing [], visible = False, datePicker = datePicker } ! [ Cmd.map SetDatePicker datePickerCmd ]


type Msg
    = Open
    | Close
    | UpdateText String
    | Submit
    | Submitted (Result Http.Error String)
    | SetDatePicker DatePicker.Msg


update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    let
        _ =
            Debug.log "msg" msg
    in
        case msg of
            Open ->
                { state | visible = True } ! []

            Close ->
                { state | visible = False } ! []

            UpdateText newText ->
                let
                    oldReminder =
                        state.reminder

                    newReminder =
                        { oldReminder | remindText = newText }
                in
                    { state | reminder = newReminder } ! []

            Submit ->
                let
                    postReminderCmd =
                        Http.send Submitted (postReminder state.reminder)
                in
                    state ! [ postReminderCmd ]

            SetDatePicker dateMsg ->
                let
                    ( newDatePicker, datePickerCmd, dateEvent ) =
                        DatePicker.update DatePicker.defaultSettings dateMsg state.datePicker

                    date =
                        case dateEvent of
                            DatePicker.NoChange ->
                                state.reminder.remindDate

                            DatePicker.Changed newDate ->
                                newDate

                    oldReminder =
                        state.reminder

                    newReminder =
                        { oldReminder | remindDate = date }
                in
                    { state
                        | reminder = newReminder
                        , datePicker = newDatePicker
                    }
                        ! [ Cmd.map SetDatePicker datePickerCmd ]

            Submitted result ->
                state ! []


view : State -> Html Msg
view { reminder, visible, datePicker } =
    div [ classList [ ( "visible", visible ), ( "reminder-container", True ) ] ]
        [ div [ class "reminder" ]
            [ div [ class "reminder-title" ] [ text "New Reminder" ]
            , div [ class "input-group" ]
                [ div [ class "input-title" ] [ text "Text" ]
                , textarea [ placeholder "Remind me to...", value reminder.remindText, onInput UpdateText ] []
                ]
            , div [ class "input-group" ]
                [ div [ class "input-title" ] [ text "Date" ]
                , DatePicker.view reminder.remindDate DatePicker.defaultSettings datePicker
                    |> Html.map SetDatePicker
                ]
            , div [ class "buttons" ]
                [ div [ class "button", onClick Close ] [ text "Close" ]
                , div [ class "button button-submit", onClick Submit ] [ text "Submit" ]
                ]
            ]
        ]
