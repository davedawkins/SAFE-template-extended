module Server

open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open Elmish.Bridge
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
            for todo in Todo.initial do
                this.AddTodo(todo) |> ignore

    member __.ClearCompleted() =
        todos.Delete ( fun x -> x.Completed ) |> ignore

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

open Shared.Remote
open Elmish
open Giraffe.Core

// server state is what the server keeps track of
type ServerState = Nothing

// the server message is what the server reacts to
// in this case, it reacts to messages from client
type ServerMsg = ClientMsg of RemoteClientMsg

// The postsHub keeps track of connected clients and has broadcasting logic
let postsHub = ServerHub<ServerState, ServerMsg, RemoteServerMsg>().RegisterServer(ClientMsg)

// react to messages coming from client
let update currentClientDispatch (ClientMsg clientMsg) currentState =
    match clientMsg with
    // when a post is added
    | TodoAdded -> 
        // tell all clients to reload posts
        postsHub.BroadcastClient ReloadTodos
        currentState, Cmd.none

// Don't do anything initially
let init (clientDispatch : Dispatch<RemoteServerMsg>) () = Nothing, Cmd.none

let socketServer =
    Bridge.mkServer Shared.Remote.endpoint init update
    |> Bridge.withConsoleTrace
    |> Bridge.withServerHub postsHub
    // register the types we can receive
    |> Bridge.register ClientMsg
    |> Bridge.run Giraffe.server


let webApp =
    Remoting.createApi()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.fromValue (Environment.databaseFilePath |> database |> Storage |> todosApi)
    |> Remoting.buildHttpHandler

let myRouter = 
    choose [
        socketServer
        webApp
    ]
    
let app =
    application {
        url "http://0.0.0.0:8085"
        use_router myRouter
        memory_cache
        use_static "public"
        use_gzip
        app_config Giraffe.useWebSockets
    }

run app
