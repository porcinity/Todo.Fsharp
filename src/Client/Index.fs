module Index

open System
open Elmish
open Fable.Remoting.Client
open Feliz
open Feliz.Bulma
open Shared

type TodoBeingEdited = {
    Id: Guid
    Description: string
}

type State = {
    Todos: Todo list
    Input: string
    TodoBeingEdited: TodoBeingEdited option
}

type Msg =
    | GotTodos of Todo list
    | SetInput of string
    | AddTodo
    | AddedTodo of Todo
    | UpdateStatus of Guid
    | UpdatedStatus of Todo
    | ClearTodos
    | ClearedTodos of Todo list
    | DeleteTodo of Guid
    | DeletedTodo of Todo
    | CancelEdit
    | ApplyEdit
    | StartEditingTodo of Guid
    | SetEditedDescription of string
    | PreviewTodo of string
    | Filter of string

let todosApi =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<ITodosApi>

let init () : State * Cmd<Msg> =
    let model = { Todos = []; Input = ""; TodoBeingEdited = None }

    let cmd =
        Cmd.OfAsync.perform todosApi.getTodos () GotTodos

    model, cmd

let withCycledTodo model todoId =
    let cycledStatus =
            model.Todos
            |> List.map (fun x ->
                match x.Id = todoId with
                | true ->
                    match x.Status with
                    | Incomplete -> { x with Status = Completed }
                    | Completed -> { x with Status = Incomplete }
                | false -> x)
    { model with Todos = cycledStatus}, Cmd.none

let withoutTodo model todoId =
    let todo =
        model.Todos
        |> List.find (fun x -> x.Id = todoId)
    { model with Input = "" }, Cmd.OfAsync.perform todosApi.deleteTodo todo DeletedTodo

let withUpdatedStatus model (todoId: Guid) =
    let todo =
        model.Todos
        |> List.find (fun x -> x.Id = todoId)
    model, Cmd.OfAsync.perform todosApi.updateStatus todo UpdatedStatus

let update (msg: Msg) (state: State) : State * Cmd<Msg> =
    match msg with
    | GotTodos todos -> { state with Todos = todos }, Cmd.none
    | SetInput value -> { state with Input = value }, Cmd.none
    | AddTodo ->
        let todo = Todo.create state.Input

        let cmd =
            Cmd.OfAsync.perform todosApi.addTodo todo AddedTodo

        { state with Input = "" }, cmd
    | AddedTodo todo ->
        { state with
              Todos = state.Todos @ [ todo ] },
        Cmd.none
    | ClearTodos ->
        { state with Input = "" }, Cmd.OfAsync.perform todosApi.deleteTodos () ClearedTodos
    | ClearedTodos list ->
        { state with Todos = list }, Cmd.none
    | DeleteTodo todoId ->
        todoId
        |> withoutTodo state
    | DeletedTodo todo ->
        let todos =
            state.Todos
            |> List.filter (fun t -> t.Id <> todo.Id)
        { state with Todos = todos }, Cmd.none
    | StartEditingTodo todoId ->
        let nextEditModel =
            state.Todos
            |> List.tryFind (fun todo -> todo.Id = todoId)
            |> Option.map (fun todo -> { Id = todoId; Description = todo.Description })
        { state with TodoBeingEdited = nextEditModel }, Cmd.none
    | CancelEdit ->
        { state with TodoBeingEdited = None }, Cmd.none
    | ApplyEdit ->
        match state.TodoBeingEdited with
        | None -> state, Cmd.none
        | Some todoBeingEdited when todoBeingEdited.Description = "" -> state, Cmd.none
        | Some todoBeingEdited ->
            let nextTodoList =
                state.Todos
                |> List.map (fun todo ->
                    if todo.Id = todoBeingEdited.Id
                    then { todo with Description = todoBeingEdited.Description }
                    else todo)
            { state with Todos = nextTodoList; TodoBeingEdited = None }, Cmd.none
    | SetEditedDescription newText ->
        let nextEditModel =
            state.TodoBeingEdited
            |> Option.map (fun todoBeingEdited -> { todoBeingEdited with Description = newText })
        { state with TodoBeingEdited = nextEditModel }, Cmd.none
    | PreviewTodo string ->
        let preview = Todo.create state.Input
        if preview.Description = "" then
            ""
        else
            preview.Description
        let newlist = state.Todos @ [ preview ]
        { state with Todos = newlist }, Cmd.none
    | Filter query ->
        let filtered =
            state.Todos
            |> List.filter (fun t -> t.Description.StartsWith(query))
        { state with Todos = filtered }, Cmd.none
    | UpdateStatus todoId ->
        todoId
        |> withUpdatedStatus state
    | UpdatedStatus todo ->
        let newList =
            state.Todos
            |> List.map (fun x ->
                if x.Id = todo.Id
                then
                    match x.Status with
                    | Incomplete -> { x with Status = Completed }
                    | Completed -> { x with Status = Incomplete }
                else x)
        { state with Todos = newList }, Cmd.none





let navBrand =
    Bulma.navbarBrand.div [
        Bulma.navbarItem.a [
            prop.href "https://safe-stack.github.io/"
            navbarItem.isActive
            prop.children [
                Html.img [
                    prop.src "/favicon.png"
                    prop.alt "Logo"
                ]
            ]
        ]
    ]

let showStatus (todo: Todo) =
    match todo.Status with
    | Completed -> "Completed"
    | Incomplete -> "Incomplete"

let div (classes: string list) (children: ReactElement list) =
    Html.div [
        prop.classes classes
        prop.children children
    ]

let appTitle =
    Html.p [
      prop.className "title"
      prop.text "Elmish To-Do List"
    ]

let inputField (state: State) (dispatch: Msg -> unit) =
  div [ "field"; "has-addons" ] [
    div [ "control"; "is-expanded" ] [
      Html.input [
        prop.classes [ "input"; "is-medium" ]
        prop.valueOrDefault (state.Input = "")
        prop.onTextChange (SetInput >> dispatch)
        prop.onTextChange (fun x -> dispatch <| Filter x )
      ]
    ]

    div [ "control" ] [
      Html.button [
        prop.classes [ "button"; "is-primary"; "is-medium" ]
        prop.onClick (fun _ -> dispatch AddTodo)
        prop.children [
          Html.i [ prop.classes [ "fa"; "fa-plus" ] ]
        ]
      ]
    ]
  ]

let renderTodo (todo: Todo) (dispatch: Msg -> unit) =
  div [ "block" ] [
    div [ "columns"; "is-vcentered"; "is-full"] [
      div [ "column" ] [
        Html.li [
//          prop.className "subtitle"
          prop.text todo.Description
        ]
      ]

      div [ "column"; "is-narrow" ] [
        div [ "buttons" ] [
          Html.button [
            if todo.Status = Incomplete
            then prop.className [ "button"; "is-info"; "is-default" ]
            else
                prop.className [ "button"; "is-success" ]
            prop.text (showStatus todo)
            prop.onClick (fun _ -> dispatch <| UpdateStatus todo.Id)
//            prop.children [
//              Html.i [
//                  prop.classes [ "fa"; "fa-check" ]
//              ]
//            ]
          ]

          Html.button [
            prop.classes [ "button"; "is-danger" ]
            prop.onClick (fun _ -> dispatch (DeleteTodo todo.Id))
            prop.text "Delete"
//            prop.children [
//              Html.i [ prop.classes [ "fa"; "fa-times" ] ]
//            ]
          ]
        ]
      ]
    ]
  ]

let todoList (state: State) (dispatch: Msg -> unit) =
  Html.ol [
    prop.children [
      for todo in state.Todos ->
        Html.li [ renderTodo todo dispatch ]
    ]
  ]


let containerBox (state: State) (dispatch: Msg -> unit) =
    Bulma.box [
         Bulma.content [
            Html.ol [
              for todo in state.Todos ->
                  renderTodo todo dispatch
            ]
            Bulma.field.div [
                field.isGrouped
                prop.children [
                    Bulma.control.p [
                        control.isExpanded
                        prop.children [
                            Bulma.input.text [
                                prop.value state.Input
                                prop.placeholder "What needs to be done?"
                                prop.onChange (fun x -> SetInput x |> dispatch)
//                                prop.onChange (fun x -> dispatch <| PreviewTodo x)
                                prop.onKeyUp (key.enter, fun _ -> dispatch AddTodo )
                            ]
                            if state.Input <> "" then
                                Bulma.block [
                                    prop.text $"Preview: {state.Input}"
                                ]
                        ]
                    ]
                    Bulma.control.p [
                        Bulma.button.a [
                            color.isPrimary
                            prop.disabled (Todo.isValid state.Input |> not)
                            prop.onClick (fun _ -> dispatch AddTodo)
                            prop.text "Add"
                        ]
                    ]
//                    Bulma.control.p [
//                        Bulma.button.a [
//                            color.isDanger
//                            prop.onClick (fun _ -> dispatch ClearTodos)
//                            prop.text "CLEAR"
//                        ]
//                    ]
                ]
            ]
            Bulma.control.p [
                prop.children [
                     Bulma.input.text [
                        prop.value state.Input
                        prop.placeholder "Filter todos"
                        prop.onChange (fun x -> SetInput x |> dispatch)
            //                prop.onChange (fun x -> dispatch <| PreviewTodo x)
//                        prop.onChange (fun x -> dispatch <| Filter x )
                    ]
                ]
            ]

//            if state.Input <> "" then
//                Bulma.block [
//                    prop.text $"Preview: {state.Input}"
//                ]
        ]
    ]

let view (model: State) (dispatch: Msg -> unit) =
    Bulma.hero [
        hero.isFullHeight
        color.isPrimary
        prop.style [
            style.backgroundSize "cover"
//            style.backgroundImageUrl "https://unsplash.it/1200/900?random"
            style.backgroundColor.steelBlue
            style.backgroundPosition "no-repeat center center fixed"
        ]
        prop.children [
            Bulma.heroHead [
                Bulma.navbar [
                    Bulma.container [ navBrand ]
                ]
            ]
            Bulma.heroBody [
                Bulma.container [
                    Bulma.column [
                        column.is12
//                        column.isOffset1
                        prop.children [
                            Bulma.title [
                                text.hasTextCentered
                                prop.text "Todo List (itb)"
                            ]
                            containerBox model dispatch
//                            inputField model dispatch
                        ]
                    ]
                ]
            ]
        ]
    ]
