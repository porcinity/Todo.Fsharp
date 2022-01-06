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

type Model = {
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
    | CancelEdit
    | ApplyEdit
    | StartEditingTodo of Guid
    | SetEditedDescription of string

let todosApi =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<ITodosApi>

let init () : Model * Cmd<Msg> =
    let model = { Todos = []; Input = "" }

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
    let todos =
        model.Todos
        |> List.filter (fun t -> t.Id <> todoId)
    { model with Todos = todos}, Cmd.none

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | GotTodos todos -> { model with Todos = todos }, Cmd.none
    | SetInput value -> { model with Input = value }, Cmd.none
    | AddTodo ->
        let todo = Todo.create model.Input

        let cmd =
            Cmd.OfAsync.perform todosApi.addTodo todo AddedTodo

        { model with Input = "" }, cmd
    | AddedTodo todo ->
        { model with
              Todos = model.Todos @ [ todo ] },
        Cmd.none
    | ClearTodos -> { model with Todos = [] }, Cmd.none
    | UpdateStatus id ->
        id
        |> withCycledTodo model

    | DeleteTodo todoId ->
        todoId
        |> withoutTodo model


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

let containerBox (model: Model) (dispatch: Msg -> unit) =
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

let view (model: Model) (dispatch: Msg -> unit) =
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
