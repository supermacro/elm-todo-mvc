'use strict';

require('sanitize.css');
require('./init.css');

var Elm = require('./Main.elm');

var mountNode = document.getElementById('app');

const initialTodos = JSON.parse(
  localStorage.getItem('todos')
) || []

// .embed() can take an optional second argument.
// This would be an object describing the data we need to start a program
// i.e. a userID or some token
var app = Elm.Main.embed(mountNode, {
  todos: initialTodos
});

app.ports.saveTodo.subscribe((todo) => {
  const todos = JSON.parse(
    localStorage.getItem('todos')
  ) || []

  todos.unshift(todo)

  localStorage.setItem('todos', JSON.stringify(todos))
})

app.ports.removeTodo.subscribe((todoId) => {
  const todos = JSON.parse(
    localStorage.getItem('todos')
  )

  const updatedTodos = todos.filter((todo) => todo.id !== todoId)

  localStorage.setItem('todos', JSON.stringify(updatedTodos))
})

app.ports.updateTodos.subscribe((todos) => {
  localStorage.setItem('todos', JSON.stringify(todos))
})
