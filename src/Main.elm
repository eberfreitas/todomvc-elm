module Main exposing (..)

import Browser
import Browser.Dom as Dom
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import Task


type alias Model =
    { uid : Int
    , newInput : String
    , entries : List Todo
    }


type alias Todo =
    { id : Int
    , title : String
    , tmpTitle : String
    , completed : Bool
    , editing : Bool
    }


type Msg
    = NoOp
    | GotTodo String
    | SubmitTodo
    | DeleteTodo Int
    | ToggleCompleted Int Bool
    | StartEditing Int
    | UpdateTodo Int String
    | SaveTodo Int
    | CancelUpdate Int


mainInput : String -> Html Msg
mainInput input_ =
    input
        [ attribute "autofocus" ""
        , class "new-todo"
        , placeholder "What needs to be done?"
        , value input_
        , onInput GotTodo
        , onEnter SubmitTodo
        ]
        []


todoItem : Todo -> Html Msg
todoItem todo =
    let
        checked =
            if todo.completed then
                [ attribute "checked" "" ]

            else
                []
    in
    li
        [ classList
            [ ( "completed", todo.completed )
            , ( "editing", todo.editing )
            ]
        , onDoubleClick (StartEditing todo.id)
        ]
        [ div [ class "view" ]
            [ input ([ class "toggle", type_ "checkbox", onClick (ToggleCompleted todo.id (not todo.completed)) ] ++ checked)
                []
            , label []
                [ text todo.title ]
            , button [ class "destroy", onClick (DeleteTodo todo.id) ]
                []
            ]
        , todoEditor todo
        ]


todoEditor : Todo -> Html Msg
todoEditor todo =
    input
        [ class "edit"
        , value todo.tmpTitle
        , id ("entry-" ++ String.fromInt todo.id)
        , onInput (UpdateTodo todo.id)
        , onBlur (CancelUpdate todo.id)
        , onEnter (SaveTodo todo.id)
        ]
        []


mainTodos : List Todo -> Html Msg
mainTodos todos =
    if List.length todos > 0 then
        section [ class "main" ]
            [ input [ class "toggle-all", id "toggle-all", type_ "checkbox" ]
                []
            , label [ for "toggle-all" ]
                [ text "Mark all as complete" ]
            , ul [ class "todo-list" ]
                (List.map todoItem todos)
            ]

    else
        text ""


view : Model -> Html Msg
view model =
    section [ class "todoapp" ]
        [ header [ class "header" ]
            [ h1 []
                [ text "todos" ]
            , mainInput model.newInput
            ]
        , mainTodos model.entries
        , footer [ class "footer" ]
            [ span [ class "todo-count" ]
                [ strong []
                    [ text "0" ]
                , text "item left"
                ]
            , ul [ class "filters" ]
                [ li []
                    [ a [ class "selected", href "#/" ]
                        [ text "All" ]
                    ]
                , li []
                    [ a [ href "#/active" ]
                        [ text "Active" ]
                    ]
                , li []
                    [ a [ href "#/completed" ]
                        [ text "Completed" ]
                    ]
                ]
            , button [ class "clear-completed" ]
                [ text "Clear completed" ]
            ]
        ]


onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Json.succeed msg

            else
                Json.fail "not ENTER"
    in
    on "keydown" (Json.andThen isEnter keyCode)


init : () -> ( Model, Cmd Msg )
init () =
    ( { uid = 0, newInput = "", entries = [] }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GotTodo input ->
            ( { model | newInput = input }, Cmd.none )

        SubmitTodo ->
            let
                newTodo =
                    String.trim model.newInput

                ( entries, id ) =
                    case newTodo of
                        "" ->
                            ( model.entries, model.uid )

                        _ ->
                            ( model.entries ++ [ Todo (model.uid + 1) model.newInput model.newInput False False ]
                            , model.uid + 1
                            )
            in
            ( { model | newInput = "", entries = entries, uid = id }, Cmd.none )

        DeleteTodo id ->
            let
                newEntries =
                    List.filter
                        (.id >> (/=) id)
                        model.entries
            in
            ( { model | entries = newEntries }
            , Cmd.none
            )

        ToggleCompleted id completed ->
            let
                newEntries =
                    List.map
                        (\entry ->
                            if entry.id == id then
                                { entry | completed = completed }

                            else
                                entry
                        )
                        model.entries
            in
            ( { model | entries = newEntries }, Cmd.none )

        StartEditing id ->
            let
                newEntries =
                    List.map
                        (\entry ->
                            if entry.id == id then
                                { entry | editing = True }

                            else
                                { entry | editing = False }
                        )
                        model.entries
            in
            ( { model | entries = newEntries }
            , Task.attempt (\_ -> NoOp) (Dom.focus ("entry-" ++ String.fromInt id))
            )

        UpdateTodo id title ->
            let
                newEntries =
                    List.map
                        (\entry ->
                            if entry.id == id then
                                { entry | tmpTitle = title }

                            else
                                entry
                        )
                        model.entries
            in
            ( { model | entries = newEntries }, Cmd.none )

        SaveTodo id ->
            let
                newEntries =
                    List.filterMap
                        (\entry ->
                            if entry.id == id then
                                let
                                    title =
                                        String.trim entry.tmpTitle
                                in
                                case title of
                                    "" ->
                                        Nothing

                                    _ ->
                                        Just { entry | title = title, editing = False }

                            else
                                Just entry
                        )
                        model.entries
            in
            ( { model | entries = newEntries }, Cmd.none )

        CancelUpdate id ->
            let
                newEntries =
                    List.map
                        (\entry ->
                            if entry.id == id then
                                { entry | tmpTitle = entry.title, editing = False }

                            else
                                entry
                        )
                        model.entries
            in
            ( { model | entries = newEntries }, Cmd.none )


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
