module Main exposing (..)

import Html exposing (Html, text, div, h1, ul, li, input, Attribute)
import Html.Attributes exposing (type_, placeholder)
import Html.Events exposing (on, keyCode, onInput)
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
    , input : String
    }



-- MSG

type Msg
    = KeyDown Int
    | Input String



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
  div [ ]
    [ h1 [] [ text "todos" ]
    , todoForm model
    ]



todoForm : Model -> Html Msg
todoForm model =
  div []
    [ todoHeader
    , todoList model
    ]


todoHeader : Html Msg
todoHeader =
  div []
    [ input [ type_ "checkbox" ] []
    , input
      [ type_ "text"
      , placeholder "What needs to be done?"
      , onKeyDown
      , onInput Input
      ] []
    ]


todoList : Model -> Html a
todoList { todos } =
  let
    listItems =
      if List.isEmpty todos then
        div [] []
      else
        let
          toListItem todo =
            li [] [text todo]

          list = List.map toListItem todos
        in
          ul [] list

  in
    div [] [ listItems ]


-- UPDATE


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    KeyDown int ->
      let
        enter = 13
      in
        if int == enter then
          (addTodo model, Cmd.none)
        else
          (model, Cmd.none)

    Input str ->
      ({ model | input = str }, Cmd.none)


addTodo : Model -> Model
addTodo model =
  let
    newTodos = Debug.log "TODO" (model.input :: model.todos)

  in
    { model | todos = newTodos }

