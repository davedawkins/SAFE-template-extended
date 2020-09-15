namespace Shared

open System

type TodoKey = Guid

[<CLIMutable>]
type Todo =
    { Id : TodoKey
      Description : string 
      Completed : bool
      }

module Todo =
    let isValid (description: string) =
        String.IsNullOrWhiteSpace description |> not

    let create (description: string) =
        { Id = Guid.NewGuid()
          Description = description
          Completed = false
          }

    let initial = [
            create "Create new SAFE project"
            create "Write your app"
            create "Ship it !!!"
        ]


module Route =
    let builder typeName methodName =
        sprintf "/api/%s/%s" typeName methodName

type ITodosApi =
    { getTodos : unit -> Async<Todo list>
      addTodo : Todo -> Async<Todo> 
      updateTodo : Todo -> Async<Todo>
      clearCompletedTodos : unit -> Async<Todo list>
      deleteTodo : Todo -> Async<TodoKey>
      }

module Remote =

    let endpoint = "/shared"

    type RemoteServerMsg = ReloadTodos
    type RemoteClientMsg = TodoAdded