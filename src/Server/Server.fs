module Server

open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open Saturn
open LiteDB.FSharp
open LiteDB

open Shared

let database dbFile =
    let mapper = FSharpBsonMapper()
    let dbFile = Environment.databaseFilePath
    let connStr = sprintf "Filename=%s;mode=Exclusive" dbFile
    new LiteDatabase( connStr, mapper )

type Storage (db : LiteDatabase) as this =
    let collection = "todos"
    let todos = db.GetCollection<Todo> collection

    do
        if not (db.CollectionExists collection) then
            this.AddTodo(Todo.create "Create new SAFE project") |> ignore
            this.AddTodo(Todo.create "Write your app") |> ignore
            this.AddTodo(Todo.create "Ship it !!!") |> ignore

    member __.ClearCompleted() =
        let n = todos.Delete ( fun x -> x.Completed )
        System.Console.WriteLine("Deleted {0} items", n)

    member __.GetTodos () =
        todos.FindAll() |> List.ofSeq

    member __.AddTodo (todo: Todo) =
        if Todo.isValid todo.Description then
            todos.Insert(todo) |> ignore
            Ok ()
        else Error "Invalid todo"

    member __.UpdateTodo (todo: Todo) =
        if Todo.isValid todo.Description then
            todos.Update( [ todo ] ) |> ignore
            Ok ()
        else Error "Invalid todo"
 
    member __.Delete (todo: Todo) =
        todos.Delete ( fun x -> x.Id = todo.Id) |> ignore

let todosApi (storage : Storage)=
    { getTodos = fun () -> async { return storage.GetTodos() }
      clearCompletedTodos = 
        fun () -> async { 
            storage.ClearCompleted()
            return storage.GetTodos()
        }
      addTodo =
        fun todo -> async {
            match storage.AddTodo todo with
            | Ok () -> return todo
            | Error e -> return failwith e
        } 
      updateTodo =
        fun todo -> async {
            System.Console.WriteLine("Server: updateToDo ")
            match storage.UpdateTodo todo with
            | Ok () -> return todo
            | Error e -> return failwith e
        }
      deleteTodo =
        fun todo -> async {
            storage.Delete todo
            return todo.Id
        }
    }

let webApp =
    Remoting.createApi()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.fromValue (Environment.databaseFilePath |> database |> Storage |> todosApi)
    |> Remoting.buildHttpHandler

let app =
    application {
        url "http://0.0.0.0:8085"
        use_router webApp
        memory_cache
        use_static "public"
        use_gzip
    }

run app
