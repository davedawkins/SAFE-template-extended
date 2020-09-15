module TodoApp.State
open Types
open Elmish
open Shared
open Shared.Remote
open Elmish.Bridge

let init (api:ITodosApi): Model * Cmd<Msg> =
    let model =
        { Todos = []
          Input = "" 
          Filter = All
          Editing = None
          }
    let cmd = Cmd.OfAsync.perform api.getTodos () GotTodos
    model, cmd

// Linear searching! There are better ways of doing this in F# I'm sure.
// In C# I'd use a Dictionary<Guid,Todo>. That's still a mutable mindset though.
let replaceTodo todos todo =
    List.map (fun t -> if todo.Id = t.Id then todo else t) todos

let updateEdit (msg : EditMsg) (model : EditModel option) : EditModel option * Cmd<EditMsg> * EditStatus =
    match model with
    | None -> None, Cmd.none, Continue
    | Some edit ->
        match msg with
        | SetEditInput s ->
            Some { edit with Input = s }, Cmd.none, Continue
        | Commit -> // Nothing to do at this level, signal parent to finish
            None, Cmd.none, Finish edit

let update (api : ITodosApi) (msg: Msg) (model: Model): Model * Cmd<Msg> =
    match msg with

    | ServerMsg msg ->
        match msg with 
        | ReloadTodos ->
            let cmd = Cmd.OfAsync.perform api.getTodos () GotTodos
            model, cmd
            
    | EditMessage em ->
        let (editState, cmd, status) = updateEdit em model.Editing
        match status with
        | Continue ->
            { model with Editing = editState }, Cmd.map EditMessage cmd
        | Finish edit ->
            { model with Editing = editState },
            Cmd.batch [ 
                Cmd.map EditMessage cmd
                Cmd.ofMsg <| UpdateTodo { edit.Todo with Description = edit.Input } 
            ]

    | StartEdit (todo) ->
        { model with Editing = Some { Todo = todo; Input = todo.Description } }, Cmd.none
 
    // Note we don't update the model here. We will only allow a message
    // from the server to update the model
    | SetCompleted (todo,completed) ->
        model, Cmd.ofMsg (UpdateTodo { todo with Completed = completed })

    | GotTodos todos ->
        { model with Todos = todos }, Cmd.none

    | SetInput value ->
        { model with Input = value }, Cmd.none

    // Exit point to server for a Todo update. Again, model is unchange until
    // we get a response
    | UpdateTodo todo ->
        let cmd = Cmd.OfAsyncImmediate.perform api.updateTodo todo UpdatedTodo
        model, cmd

    // Finally, we can safely update our model - the server is supplying
    // state (and we assume the server has persisted that state successfully)
    | UpdatedTodo todo ->
        { model with Todos = replaceTodo model.Todos todo  }, Cmd.bridgeSend (RemoteClientMsg.TodoAdded)
 
    // Todo not added to model here, until server responds 
    | AddTodo ->
        let todo = Todo.create model.Input
        let cmd = Cmd.OfAsync.perform api.addTodo todo AddedTodo
        { model with Input = "" }, cmd

    // Now we can add the todo
    | AddedTodo todo ->
        { model with Todos = model.Todos @ [ todo ] }, Cmd.bridgeSend (RemoteClientMsg.TodoAdded)

    | ClearCompleted -> 
        model, Cmd.OfAsync.perform api.clearCompletedTodos () GotTodos

    | DeleteTodo todo ->
        model, Cmd.OfAsync.perform api.deleteTodo todo DeletedTodo
 
    | DeletedTodo key ->
        { model with Todos = model.Todos |> List.filter (fun x -> x.Id <> key) }, Cmd.none

    | SetFilter f ->
        { model with Filter = f }, Cmd.none

