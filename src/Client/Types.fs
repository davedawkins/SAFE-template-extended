module TodoApp.Types

open Shared

type FilterOptions = 
    | All
    | Active
    | Complete

// Editing an existing todo
type EditMsg =
    | SetEditInput of string
    | Commit

// State for editing a todo
type EditModel =
    {
        Todo: Todo
        Input: string
    }

// How updateEdit tells parent (update) what to do 
type EditStatus =
    | Continue
    | Finish of EditModel

// Global UI state
type Model =
    { 
      Todos: Todo list
      Input: string 
      Filter: FilterOptions
      Editing: EditModel option  // Some when editing, None when not.
    }

type Msg =
    | GotTodos of Todo list
    | SetInput of string
    | SetCompleted of Todo * bool
    | AddTodo
    | AddedTodo of Todo
    | UpdateTodo of Todo
    | UpdatedTodo of Todo
    | DeleteTodo of Todo
    | DeletedTodo of TodoKey
    | ClearCompleted
    | SetFilter of FilterOptions
    | StartEdit of Todo
    | EditMessage of EditMsg
