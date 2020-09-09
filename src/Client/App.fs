module App

open Elmish
open Elmish.React

#if DEBUG
open Elmish.Debug
open Elmish.HMR
open Fable.Remoting.Client
open Shared
#endif

let useMemory = false

// TODO: No really, this function still todo
let inMemoryTodosApi = 
    let mutable todos : Todo list = []
    {
        getTodos = fun () -> async { return todos } // : unit -> Async<Todo list>

        addTodo  = fun todo -> async { 
            todos <- todo :: todos
            return todo 
        } // : Todo -> Async<Todo> 

        updateTodo = fun todo -> async {
            todos <- todos |> List.map (fun x -> if x.Id = todo.Id then todo else x)
            return todo 
        } // : Todo -> Async<Todo>

        clearCompletedTodos = fun () -> async { 
            todos <- todos |> List.filter (fun x -> not x.Completed)
            return todos 
        } // : unit -> Async<Todo list>

        deleteTodo = fun todo -> async { 
            todos <- todos |> List.filter (fun x -> x.Id <> todo.Id)
            return todo.Id 
        } // : Todo -> Async<TodoKey>
    }

let remoteTodosApi =
    Remoting.createApi()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<ITodosApi>

let todosApi =
    if useMemory then remoteTodosApi else inMemoryTodosApi

Program.mkProgram (fun() -> TodoApp.State.init todosApi) (TodoApp.State.update todosApi) TodoApp.View.view
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReactSynchronous "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
