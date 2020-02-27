port module Main exposing (..)

import Browser
import Browser.Dom as Dom
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Html.Lazy exposing (..)
import Json.Decode as Json
import Json.Encode as E
import Task


port sendModel : E.Value -> Cmd msg


type alias Model =
    { uid : Int
    , newInput : String
    , entries : List Todo
    , activeView : ActiveView
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
    | ToggleAll Bool
    | StartEditing Int
    | UpdateTodo Int String
    | SaveTodo Int
    | CancelUpdate Int
    | ClearCompleted
    | ChangeActiveView ActiveView


type ActiveView
    = All
    | Active
    | Completed


decoder : Json.Decoder Model
decoder =
    Json.map4 Model
        (Json.field "uid" Json.int)
        (Json.field "new_input" Json.string)
        (Json.field "entries" (Json.list entryDecoder))
        (Json.field "active_view" activeViewDecoder)


entryDecoder : Json.Decoder Todo
entryDecoder =
    Json.map5 Todo
        (Json.field "id" Json.int)
        (Json.field "title" Json.string)
        (Json.field "tmpTitle" Json.string)
        (Json.field "completed" Json.bool)
        (Json.field "editing" Json.bool)


activeViewDecoder : Json.Decoder ActiveView
activeViewDecoder =
    Json.string
        |> Json.andThen
            (\activeView ->
                case activeView of
                    "all" ->
                        Json.succeed All

                    "active" ->
                        Json.succeed Active

                    "completed" ->
                        Json.succeed Completed

                    _ ->
                        Json.fail ("Unknown active view value of " ++ activeView)
            )


encoder : Model -> E.Value
encoder model =
    E.object
        [ ( "uid", E.int model.uid )
        , ( "new_input", E.string model.newInput )
        , ( "entries", E.list entryEncoder model.entries )
        , ( "active_view", activeViewEncoder model.activeView )
        ]


activeViewEncoder : ActiveView -> E.Value
activeViewEncoder activeView =
    case activeView of
        All ->
            E.string "all"

        Active ->
            E.string "active"

        Completed ->
            E.string "completed"


entryEncoder : Todo -> E.Value
entryEncoder todo =
    E.object
        [ ( "id", E.int todo.id )
        , ( "title", E.string todo.title )
        , ( "tmpTitle", E.string todo.tmpTitle )
        , ( "completed", E.bool todo.completed )
        , ( "editing", E.bool todo.editing )
        ]


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


todoItem : Todo -> ( String, Html Msg )
todoItem todo =
    ( "entry-" ++ String.fromInt todo.id
    , li
        [ classList
            [ ( "completed", todo.completed )
            , ( "editing", todo.editing )
            ]
        , onDoubleClick (StartEditing todo.id)
        ]
        [ div [ class "view" ]
            [ input
                [ class "toggle"
                , type_ "checkbox"
                , checked todo.completed
                , onClick (ToggleCompleted todo.id (not todo.completed))
                ]
                []
            , label []
                [ text todo.title ]
            , button [ class "destroy", onClick (DeleteTodo todo.id) ]
                []
            ]
        , todoEditor todo
        ]
    )


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


mainTodos : ActiveView -> List Todo -> Html Msg
mainTodos activeView todos =
    let
        todos_ =
            case activeView of
                All ->
                    todos

                Active ->
                    List.filter (.completed >> (==) False) todos

                Completed ->
                    List.filter (.completed >> (==) True) todos

        allCompleted =
            List.all (.completed >> (==) True) todos
    in
    if List.length todos_ > 0 then
        section [ class "main" ]
            [ input
                [ class "toggle-all"
                , id "toggle-all"
                , type_ "checkbox"
                , checked allCompleted
                , onCheck ToggleAll
                ]
                []
            , label [ for "toggle-all" ]
                [ text "Mark all as complete" ]
            , Keyed.ul [ class "todo-list" ]
                (List.map todoItem todos_)
            ]

    else
        text ""


footer_ : ActiveView -> List Todo -> Html Msg
footer_ activeView todos =
    let
        itemsCount =
            todos
                |> List.filter (.completed >> (/=) True)
                |> List.length

        itemsLabel =
            case itemsCount of
                1 ->
                    " item"

                _ ->
                    " items"

        alwaysPreventDefault : msg -> ( msg, Bool )
        alwaysPreventDefault msg =
            ( msg, True )

        filterHandler : ActiveView -> Attribute Msg
        filterHandler active =
            preventDefaultOn
                "click"
                (Json.map alwaysPreventDefault (Json.succeed (ChangeActiveView active)))
    in
    if List.length todos /= 0 then
        footer [ class "footer" ]
            [ span [ class "todo-count" ]
                [ strong []
                    [ text (String.fromInt itemsCount) ]
                , text (itemsLabel ++ " left")
                ]
            , ul [ class "filters" ]
                [ li []
                    [ a
                        [ classList [ ( "selected", activeView == All ) ]
                        , href "#/"
                        , filterHandler All
                        ]
                        [ text "All" ]
                    ]
                , li []
                    [ a
                        [ classList [ ( "selected", activeView == Active ) ]
                        , href "#/active"
                        , filterHandler Active
                        ]
                        [ text "Active" ]
                    ]
                , li []
                    [ a
                        [ classList [ ( "selected", activeView == Completed ) ]
                        , href "#/completed"
                        , filterHandler Completed
                        ]
                        [ text "Completed" ]
                    ]
                ]
            , button
                [ class "clear-completed"
                , onClick ClearCompleted
                ]
                [ text "Clear completed" ]
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
        , mainTodos model.activeView model.entries
        , footer_ model.activeView model.entries
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


init : String -> ( Model, Cmd Msg )
init model =
    let
        model_ =
            case Json.decodeString decoder model of
                Ok m ->
                    m

                Err _ ->
                    { uid = 0, newInput = "", entries = [], activeView = All }
    in
    ( model_
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
                entries =
                    List.filter
                        (.id >> (/=) id)
                        model.entries
            in
            ( { model | entries = entries }
            , Cmd.none
            )

        ToggleCompleted id completed ->
            let
                entries =
                    List.map
                        (\entry ->
                            if entry.id == id then
                                { entry | completed = completed }

                            else
                                entry
                        )
                        model.entries
            in
            ( { model | entries = entries }, Cmd.none )

        ToggleAll completed ->
            let
                entries =
                    List.map
                        (\entry -> { entry | completed = completed })
                        model.entries
            in
            ( { model | entries = entries }, Cmd.none )

        StartEditing id ->
            let
                entries =
                    List.map
                        (\entry ->
                            if entry.id == id then
                                { entry | editing = True }

                            else
                                { entry | editing = False }
                        )
                        model.entries
            in
            ( { model | entries = entries }
            , Task.attempt (\_ -> NoOp) (Dom.focus ("entry-" ++ String.fromInt id))
            )

        UpdateTodo id title ->
            let
                entries =
                    List.map
                        (\entry ->
                            if entry.id == id then
                                { entry | tmpTitle = title }

                            else
                                entry
                        )
                        model.entries
            in
            ( { model | entries = entries }, Cmd.none )

        SaveTodo id ->
            let
                entries =
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
            ( { model | entries = entries }, Cmd.none )

        CancelUpdate id ->
            let
                entries =
                    List.map
                        (\entry ->
                            if entry.id == id then
                                { entry | tmpTitle = entry.title, editing = False }

                            else
                                entry
                        )
                        model.entries
            in
            ( { model | entries = entries }, Cmd.none )

        ClearCompleted ->
            let
                entries =
                    List.filter
                        (.completed >> (/=) True)
                        model.entries
            in
            ( { model | entries = entries }, Cmd.none )

        ChangeActiveView activeView ->
            ( { model | activeView = activeView }, Cmd.none )


updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg model =
    let
        ( model_, cmd_ ) =
            update msg model

        storageCmd =
            model_
                |> encoder
                |> sendModel
    in
    ( model_, Cmd.batch [ cmd_, storageCmd ] )


main =
    Browser.element
        { init = init
        , view = view
        , update = updateWithStorage
        , subscriptions = \_ -> Sub.none
        }
