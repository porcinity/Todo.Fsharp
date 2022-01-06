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
    | ClearTodos
    | DeleteTodo of Guid
    | DeletedTodo of Todo list
    | CancelEdit
    | ApplyEdit
    | StartEditingTodo of Guid
    | SetEditedDescription of string

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
//    let todos =
//        model.Todos
//        |> List.filter (fun t -> t.Id <> todoId)
    { model with Input = "" }, Cmd.OfAsync.perform todosApi.deleteTodo todo DeletedTodo

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
    | ClearTodos -> { state with Todos = [] }, Cmd.none
    | UpdateStatus id ->
        id
        |> withCycledTodo state

    | DeleteTodo todoId ->
        todoId
        |> withoutTodo state
    | DeletedTodo todoList ->
//        let newState = state.Todos |> List.filter (fun x -> x.id <> 
        { state with Todos = todoList }, Cmd.none
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

let showStatus todo =
    match todo.Status with
    | Completed -> "Completed"
    | Incomplete -> "Incomplete"

let containerBox (model: State) (dispatch: Msg -> unit) =
    Bulma.box [
         Bulma.content [
            Html.ol [
                for todo in model.Todos do
                    Html.li[
                        Bulma.field.div [
                            field.isGrouped
                            prop.children [
                                Bulma.block [
                                    Bulma.button.a [
                                        if todo.Status = Completed
                                        then color.isSuccess
                                        else color.isInfo
                                        prop.onClick (fun _ -> dispatch <| UpdateStatus todo.Id)
                                        prop.text (showStatus todo)
                                    ]

                                ]
                                Bulma.button.a [
                                    color.isWhite
                                    prop.onClick (fun _ -> dispatch <| DeleteTodo todo.Id)
                                    prop.text todo.Description
                                ]

                        ]
                    ]
                ]
            ]
            Bulma.field.div [
                field.isGrouped
                prop.children [
                    Bulma.control.p [
                        control.isExpanded
                        prop.children [
                            Bulma.input.text [
                                prop.value model.Input
                                prop.placeholder "What needs to be done?"
                                prop.onChange (fun x -> SetInput x |> dispatch)
                                prop.onKeyUp (key.enter, fun _ -> dispatch AddTodo )
                            ]
                        ]
                    ]
                    Bulma.control.p [
                        Bulma.button.a [
                            color.isPrimary
                            prop.disabled (Todo.isValid model.Input |> not)
                            prop.onClick (fun _ -> dispatch AddTodo)
                            prop.text "Add"
                        ]
                    ]
                    Bulma.control.p [
                        Bulma.button.a [
                            color.isDanger
                            prop.onClick (fun _ -> dispatch ClearTodos)
                            prop.text "CLEAR"
                        ]
                    ]
                ]
            ]
        ]
    ]

let view (model: State) (dispatch: Msg -> unit) =
    Bulma.hero [
        hero.isFullHeight
        color.isPrimary
        prop.style [
            style.backgroundSize "cover"
            style.backgroundImageUrl "https://unsplash.it/1200/900?random"
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
                        column.is6
                        column.isOffset3
                        prop.children [
                            Bulma.title [
                                text.hasTextCentered
                                prop.text "safe_test"
                            ]
                            containerBox model dispatch
                        ]
                    ]
                ]
            ]
        ]
    ]
