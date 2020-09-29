module App

open Elmish
open Elmish.React

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

open Fable.Remoting.Client
open Shared

// I need a way to set this true only when building for gh-pages
#if GHPAGES

let todosApi = 
    let mutable todos : Todo list = Todo.initial
    {
        getTodos = fun () -> async { return todos } // : unit -> Async<Todo list>

        addTodo  = fun todo -> async { 
            todos <- todo :: todos
            return todo 
        }

        updateTodo = fun todo -> async {
            todos <- todos |> List.map (fun x -> if x.Id = todo.Id then todo else x)
            return todo 
        }

        clearCompletedTodos = fun () -> async { 
            todos <- todos |> List.filter (fun x -> not x.Completed)
            return todos 
        }

        deleteTodo = fun todo -> async { 
            todos <- todos |> List.filter (fun x -> x.Id <> todo.Id)
            return todo.Id 
        }
    }

#else

let todosApi =
    Remoting.createApi()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<ITodosApi>

#endif

Program.mkProgram (fun() -> TodoApp.State.init todosApi) (TodoApp.State.update todosApi) TodoApp.View.view
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReactSynchronous "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
