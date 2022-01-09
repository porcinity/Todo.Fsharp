module Server

open CommonExtensionsAndTypesForNpgsqlFSharp
open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open Npgsql.FSharp
open Saturn

open Shared

let conn = "Host=localhost;Database=todos;Username=anthony;Password=itb"

type Storage() =
    let (todos: Todo ResizeArray) = ResizeArray<_>()

    member __.GetTodos(connStr: string) = async {
        let res' =
            connStr
            |> Sql.connect
            |> Sql.query "SELECT * FROM todos"
            |> Sql.executeAsync (fun read ->
                {
                    Id = read.uuid "id"
                    Description = read.string "description"
                    Status = read.string "status"
                })
        let! th = res'|> Async.AwaitTask
        return th
    }


    member __.AddTodo(todo: Todo, connStr: string) =
        if Todo.isValid todo.Description then
            let dto = TodoDto.fromTodo todo
            connStr
            |> Sql.connect
            |> Sql.query "INSERT INTO todos (id, description, status) VALUES (@id, @desc, @status)"
            |> Sql.parameters [ "@id", Sql.uuid dto.Id; "@desc", Sql.text dto.Description; "@status", Sql.text dto.Status ]
            |> Sql.executeNonQuery
            |> ignore
            Ok()
        else
            Error "Invalid todo"
    member __.UpdateStatus(todo: Todo) =
        let remove = todos.Find(fun t -> t.Id = todo.Id)
        let update = { remove with Status = Completed }
        match todos.Remove remove with
        | true ->
            todos.Add update
            Ok ()
        | false -> Error "Invalid request"
        let (newTodos: Todo list) =
            todos
            |> List.ofSeq
            |> List.map (fun x ->
                match x.Id = todo.Id with
                | true ->
                    match x.Status with
                    | Incomplete -> { x with Status = Completed }
                    | Completed -> { x with Status = Incomplete }
                | false -> x)
        let todos = newTodos
        Ok ()
    member __.DeleteTodo(todo: Todo) =
        let del =
            conn
            |> Sql.connect
            |> Sql.query "delete from todos where id = @id"
            |> Sql.parameters [ "id", Sql.uuid todo.Id ]
            |> Sql.executeNonQuery
        match del with
        | 1 -> Ok ()
        | _ -> Error "Not found"

    member __.DeleteTodos () =
        todos.Clear()
        todos


let storage = Storage()

let todosApi =
    { getTodos = fun () -> async {
            let! stuff = storage.GetTodos(conn)
            let newstuff =
                stuff
                |> List.map (fun t -> TodoDto.ofTodo t)
            return newstuff
        }
      addTodo =
          fun todo ->
              async {
                  match storage.AddTodo (todo, conn) with
                  | Ok () -> return todo
                  | Error e -> return failwith e
              }
      updateStatus =
          fun todo ->
              async {
                  match storage.UpdateStatus todo with
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
                  let! stuff = storage.GetTodos(conn)
                  let newstuff =
                      stuff
                      |> List.map (fun t -> TodoDto.ofTodo t)
                  return newstuff
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
