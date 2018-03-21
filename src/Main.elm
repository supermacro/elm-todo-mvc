module Main exposing (..)

import Html exposing (Html, text, div, h1, ul, li, input, label, button, Attribute)
import Html.Attributes exposing (class, type_, placeholder, value)
import Html.Events exposing (on, keyCode, onInput, onClick)
import Json.Decode as Json


main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }




-- MODEL
type alias Model =
    { todos : List String
    , userInput : String
    }



-- MSG

type Msg
    = KeyDown Int
    | Input String
    | Remove String



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
todoHeader { userInput } =
  div [ class "todo-header" ]
    [ input [ class "toggle-all", type_ "checkbox" ] []
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
          [ input [ class "checkbox", type_ "checkbox" ] []
          , label [] [ text todo ]
          , button [ class "remove-todo", onClick (Remove todo) ] []
          ]

      list = List.map toListItem todos
    in
      ul [] list



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

    Remove todo ->
      let
        filteredTodos = List.filter ((/=) todo) model.todos
      in
        ({ model | todos = filteredTodos }, Cmd.none)



addTodo : Model -> Model
addTodo model =
  let
    newTodos = Debug.log "TODO" (model.userInput :: model.todos)

  in
    { model | todos = newTodos, userInput = "" }

