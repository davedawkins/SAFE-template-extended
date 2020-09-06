module Index

open Elmish
open Fable.Remoting.Client
open Shared
open Browser

type FilterOptions = 
    | All
    | Active
    | Complete

type Model =
    { 
      Todos: Todo list
      Input: string 
      Filter: FilterOptions
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

let todosApi =
    Remoting.createApi()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<ITodosApi>

let init(): Model * Cmd<Msg> =
    let model =
        { Todos = []
          Input = "" 
          Filter = All
          }
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

    | SetFilter f ->
        { model with Filter = f }, Cmd.none

open Fable.React
open Fable.React.Props
open Fable.Import
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

let filterButton model dispatch label (op : FilterOptions) =
    Radio.radio [ 
        Props [ 
            Checked (model.Filter = op)
            OnChange (fun _ -> SetFilter op |> dispatch) 
        ] 
    ] [ 
        Radio.input [ 
            Radio.Input.Name "Filters" 
            Radio.Input.Props [ Checked (op = model.Filter) ]
        ]
        str label
    ]

let Centered (items : seq<ReactElement>) =
    div [ Style [ 
                Display DisplayOptions.Flex
                JustifyContent "center"
            ]
    ] items 

let todoOptionBar model (dispatch: Msg -> unit) =
    let plural label n = if n = 1 then label else label + "s"
    let n = numItemsLeft model

    Columns.columns [ ] [
        Column.column [ Column.Width ( Screen.All, Column.Is3 ); Column.CustomClass "has-text-left" ] [
            sprintf "%d %s left" n (plural "item" n) |> str
        ]
        Column.column [ 
            Column.Width ( Screen.All, Column.Is6 )
        ] [
            Centered [
                filterButton model dispatch  " All" FilterOptions.All
                filterButton model dispatch  " Active" FilterOptions.Active
                filterButton model dispatch  " Complete" FilterOptions.Complete
            ]
        ]
        Column.column [ Column.Width ( Screen.All, Column.Is3 ); Column.CustomClass "has-text-right" ] [
            a [ Href "#"; OnClick (fun e -> e.preventDefault(); dispatch ClearCompleted ) ] [str  "Clear completed" ]
        ]
    ]


let todoList model dispatch =
    Content.content [ ] [
        let vis (todo : Todo) = 
            match model.Filter with
            | FilterOptions.All -> true
            | Active -> not todo.Completed
            | Complete -> todo.Completed

        for todo in model.Todos |> List.filter vis ->
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
                    ]
                    str todo.Description
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
                    Heading.h1 [ Heading.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ] [ str "todos" ]
                    containerBox model dispatch
                ]
            ]
        ]
    ]

