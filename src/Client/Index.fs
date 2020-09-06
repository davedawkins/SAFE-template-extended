module Index

open Elmish
open Fable.Remoting.Client
open Shared
open Browser

type Model =
    { 
      Todos: Todo list
      Input: string 
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

let todosApi =
    Remoting.createApi()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<ITodosApi>

let init(): Model * Cmd<Msg> =
    let model =
        { Todos = []
          Input = "" }
    let cmd = Cmd.OfAsync.perform todosApi.getTodos () GotTodos
    model, cmd

let updateTodo todos todo =
    List.map (fun t -> if todo.Id = t.Id then todo else t) todos

let update (msg: Msg) (model: Model): Model * Cmd<Msg> =
    match msg with
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
        let cmd = Cmd.OfAsync.perform todosApi.updateTodo todo UpdatedTodo
        model, cmd

    // Finally, we can safely update our model - the server is supplying
    // state (and we assume the server has persisted that state successfully)
    | UpdatedTodo todo ->
        { model with Todos = updateTodo model.Todos todo  }, Cmd.none
 
    // Todo not added to model here, until server responds 
    | AddTodo ->
        let todo = Todo.create model.Input
        let cmd = Cmd.OfAsync.perform todosApi.addTodo todo AddedTodo
        { model with Input = "" }, cmd

    // Now we can add the todo
    | AddedTodo todo ->
        { model with Todos = model.Todos @ [ todo ] }, Cmd.none

    | ClearCompleted -> 
        model, Cmd.OfAsync.perform todosApi.clearCompletedTodos () GotTodos

    | DeleteTodo todo ->
        model, Cmd.OfAsync.perform todosApi.deleteTodo todo DeletedTodo
 
    | DeletedTodo key ->
        { model with Todos = model.Todos |> List.filter (fun x -> x.Id <> key) }, Cmd.none

open Fable.React
open Fable.React.Props
open Fulma

let navBrand =
    Navbar.Brand.div [ ] [
        Navbar.Item.a [
            Navbar.Item.Props [ Href "https://safe-stack.github.io/" ]
            Navbar.Item.IsActive true
        ] [
            img [
                Src "/favicon.png"
                Alt "Logo"
            ]
        ]
    ]

let numItemsLeft (model:Model) =
    model.Todos |> List.filter (fun t -> not t.Completed) |> List.length

let OnReturn f =
    DOMAttr.OnKeyUp (fun e -> if e.keyCode = 13.0 then e.preventDefault(); f())

let todoInput model dispatch =
    Field.div [ Field.IsGrouped ] [
        Control.p [ Control.IsExpanded ] [
            Input.text [
              Input.Value model.Input
              Input.Placeholder "What needs to be done?"
              Input.OnChange (fun x -> SetInput x.Value |> dispatch)
              Input.Props [ OnReturn (fun _ -> dispatch AddTodo) ]
              ]
        ]
        //Control.p [ ] [
        //    Button.a [
        //        Button.Color IsPrimary
        //        Button.Disabled (Todo.isValid model.Input |> not)
        //        Button.OnClick (fun _ -> dispatch AddTodo)
        //    ] [
        //        str "Add"
        //    ]
        //]
    ]

let todoOptionBar model dispatch =
    Field.div [ Field.IsGrouped ] [
        sprintf "%d items left" (numItemsLeft model) |> str
        str "All"
        str "Active"
        str "Completed"
        a [ Href "#"; OnClick (fun e -> e.preventDefault(); dispatch ClearCompleted ) ] [str  "Clear completed" ]
    ]


let todoList model dispatch =
    Content.content [ ] [
        for todo in model.Todos ->
            let completeClass = if todo.Completed then [ "completed"] else []
            let itemClasses = [ "todo-item" ] @ completeClass

            Columns.columns  [ Columns.CustomClass (String.concat " " itemClasses) ] [
                Column.column [Column.Width ( Screen.All, Column.Is11 )] [
                    Checkbox.checkbox [ ] [ 
                        Checkbox.input [ 
                            Modifiers [ Modifier.Spacing (Spacing.MarginRight, Spacing.Is2) ]; 
                            CustomClass "is-normal"
                            Props [ 
                                Checked todo.Completed; 
                                OnClick (fun _ -> (todo, not todo.Completed) |> SetCompleted |> dispatch) 
                            ]
                        ] 
                        str todo.Description
                    ]
                ]
                Column.column [] [
                    button [ 
                        ClassName "destroy"
                        OnClick (fun _ -> DeleteTodo todo |> dispatch)
                    ] [ str "✕" ]
                ]
            ]
        ]


let containerBox (model : Model) (dispatch : Msg -> unit) =
    Box.box' [ 
        ] [
        todoInput model dispatch
        todoList model dispatch
        todoOptionBar model dispatch
    ]

let view (model : Model) (dispatch : Msg -> unit) =
    Hero.hero [
        Hero.Color IsPrimary
        Hero.IsFullHeight
        Hero.Props [
            Style [
                Background """linear-gradient(rgba(0, 0, 0, 0.5), rgba(0, 0, 0, 0.5)), url("https://unsplash.it/1200/900?random") no-repeat center center fixed"""
                BackgroundSize "cover"
            ]
        ]
    ] [
        Hero.head [ ] [
            Navbar.navbar [ ] [
                Container.container [ ] [ navBrand ]
            ]
        ]

        Hero.body [ ] [
            Container.container [ ] [
                Column.column [
                    Column.Width (Screen.All, Column.Is6)
                    Column.Offset (Screen.All, Column.Is3)
                ] [
                    Heading.p [ Heading.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ] [ str "safe_litedb" ]
                    containerBox model dispatch
                ]
            ]
        ]
    ]

