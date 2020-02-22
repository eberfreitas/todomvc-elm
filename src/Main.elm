module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json


type alias Model =
    { todo : String
    , todoList : List Todo
    }


type alias Todo =
    { id : Int
    , todo : String
    , completed : Bool
    }


type Msg
    = GotTodo String
    | SubmitTodo


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
    li [ classList [ ( "completed", todo.completed ) ] ]
        [ div [ class "view" ]
            [ input ([ class "toggle", type_ "checkbox" ] ++ checked)
                []
            , label []
                [ text todo.todo ]
            , button [ class "destroy" ]
                []
            ]
        , input [ class "edit", value todo.todo ]
            []
        ]


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
            , mainInput model.todo
            ]
        , mainTodos model.todoList
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
    ( { todo = "", todoList = [] }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotTodo todo ->
            ( { model | todo = todo }, Cmd.none )

        SubmitTodo ->
            let
                todoList =
                    model.todoList ++ [ Todo 0 model.todo False ]
            in
            ( { model | todo = "", todoList = todoList }, Cmd.none )


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
