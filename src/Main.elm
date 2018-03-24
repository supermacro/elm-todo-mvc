module Main exposing (..)

import Html exposing (Html, text, div, h1, ul, li, input, label, button, Attribute)
import Html.Attributes exposing (class, type_, placeholder, value, checked)
import Html.Events exposing (on, keyCode, onInput, onClick, onCheck)
import Json.Decode as Json


main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }




-- MODEL
type alias Todo =
  { completed: Bool
  , content: String
  , id: Int
  }

type alias Model =
    { todos : List Todo
    , userInput : String
    }



-- MSG

type Msg
    = KeyDown Int
    | Input String
    | Remove Int
    | Toggle Int
    | ToggleAll



-- INIT

-- js interop
-- type alias Flags =
--   { user : String
--   , token : String
--   }

-- init : Flags -> ( Model, Cmd Msg )

init : (Model, Cmd Msg)
init = (Model [] "", Cmd.none)



onKeyDown : Attribute Msg
onKeyDown =
    on "keydown" (Json.map KeyDown keyCode)


-- VIEW


view : Model -> Html Msg
view model =
  div [ class "todo-list-container" ]
    [ h1 [] [ text "todos" ]
    , todoForm model
    ]



todoForm : Model -> Html Msg
todoForm model =
  div [ class "form" ]
    [ todoHeader model
    , todoList model
    ]


todoHeader : Model -> Html Msg
todoHeader { userInput, todos } =
  let
    allChecked = not <| List.member False <| List.map (\todo -> todo.completed) todos

    toggleAll = (\_ -> ToggleAll)
  in
    div [ class "todo-header" ]
      [ input [ class "toggle-all", checked allChecked, onCheck toggleAll, type_ "checkbox" ] []
      , input
        [ class "user-input"
        , type_ "text"
        , placeholder "What needs to be done?"
        , onKeyDown
        , onInput Input
        , value userInput
        ] []
      ]


todoList : Model -> Html Msg
todoList { todos } =
  if List.isEmpty todos then
    div [] []

  else
    let
      toListItem todo =
        li [ class "list-item" ]
          [ input [ class "checkbox", type_ "checkbox", checked todo.completed, onCheck (\_ -> Toggle todo.id) ] []
          , label [] [ text todo.content ]
          , button [ class "remove-todo", onClick (Remove todo.id) ] []
          ]

      footer =
        let
          count = List.length <| List.filter (\todo -> not todo.completed) todos

          footerMsg =
            if count == 1 then
              (toString count) ++ " item left"
            else
              (toString count) ++ " items left"

        in
          div [] [ text footerMsg ]

      list = List.map toListItem todos

    in
      div []
        [ ul [] list
        , footer
        ]




-- UPDATE


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    KeyDown int ->
      let
        enterKey = 13
      in
        if int == enterKey then
          (addTodo model, Cmd.none)
        else
          (model, Cmd.none)

    Input str ->
      ({ model | userInput = str }, Cmd.none)

    Remove todoId ->
      let
        filteredTodos = List.filter (\todo -> todo.id /= todoId) model.todos
      in
        ({ model | todos = filteredTodos }, Cmd.none)

    Toggle todoId ->
      let
        updatedTodos =
          (flip List.map) model.todos
            (\todo ->
              if todo.id == todoId then
                { todo | completed = not todo.completed }
              else
                todo)
      in
        ({ model | todos = updatedTodos }, Cmd.none)


    ToggleAll ->
      let
        allToggled =
          List.map (\todo -> { todo | completed = not todo.completed }) model.todos
      in
        ({ model | todos = allToggled }, Cmd.none)



addTodo : Model -> Model
addTodo model =
  let
    newTodo =
      { completed = False
      , content = model.userInput
      , id = (List.length model.todos) + 1
      }

    newTodos = Debug.log "TODO" (newTodo :: model.todos)

  in
    { model | todos = newTodos, userInput = "" }

