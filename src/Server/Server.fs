module Server

open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open Saturn

open Shared

type Storage() =
    let todos = ResizeArray<_>()

    member __.GetTodos() = List.ofSeq todos

    member __.AddTodo(todo: Todo) =
        if Todo.isValid todo.Description then
            todos.Add todo
            Ok()
        else
            Error "Invalid todo"
    member __.DeleteTodo(todo: Todo) =
        let remove = todos.Find(fun t -> t.Id = todo.Id)
        match todos.Remove(remove) with
        | true -> Ok ()
        | false -> Error "Not found"
    member __.DeleteTodos () =
        todos.Clear()
        todos


let storage = Storage()

storage.AddTodo(Todo.create "Create new SAFE project")
|> ignore

storage.AddTodo(Todo.create "Write your app")
|> ignore

storage.AddTodo(Todo.create "Ship it !!!")
|> ignore

let todosApi =
    { getTodos = fun () -> async { return storage.GetTodos() }
      addTodo =
          fun todo ->
              async {
                  match storage.AddTodo todo with
                  | Ok () -> return todo
                  | Error e -> return failwith e
              }
      deleteTodo =
          fun todo ->
              async {
                  match storage.DeleteTodo todo with
                  | Ok () -> return todo
                  | Error e -> return failwith e
              }
      deleteTodos =
          fun () ->
              async {
                  storage.DeleteTodos () |> ignore
                  return storage.GetTodos()
              }}

let webApp =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.fromValue todosApi
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
