module Main exposing (..)

import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (src, style)
import Html.Events exposing (onClick)
import Reminder


---- MODEL ----


type alias Model =
    { reminder : Reminder.State }


init : ( Model, Cmd Msg )
init =
    let
        ( reminderState, reminderCmd ) =
            Reminder.init
    in
        ( { reminder = reminderState }, Cmd.map ReminderMsg reminderCmd )



---- UPDATE ----


type Msg
    = NoOp
    | ReminderMsg Reminder.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReminderMsg reminderMsg ->
            let
                ( reminderState, reminderCmd ) =
                    Reminder.update reminderMsg model.reminder
            in
                { model | reminder = reminderState } ! [ Cmd.map ReminderMsg reminderCmd ]

        NoOp ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text "Your Elm App is working!" ]
        , div [ style [ ( "cursor", "pointer" ) ], onClick <| ReminderMsg Reminder.Open ] [ text " Open Reminder" ]
        , Html.map ReminderMsg <| Reminder.view model.reminder
        ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
