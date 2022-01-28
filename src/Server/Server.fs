module Server

open CommonExtensionsAndTypesForNpgsqlFSharp
open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open Npgsql.FSharp
open Saturn

open Shared

let conn = "Host=localhost;Database=todos;Username=pigg"

type Storage() =

    member __.GetTodos(connStr: string) = async {
        let! res =
            connStr
            |> Sql.connect
            |> Sql.query "SELECT * FROM todos"
            |> Sql.executeAsync (fun read ->
                {
                    Id = read.uuid "id"
                    Description = read.string "description"
                    Status = read.string "status"
                })
            |> Async.AwaitTask
        return res
    }


    member __.AddTodo(todo: Todo, connStr: string) = async {
        let res =
            if Todo.isValid todo.Description
            then
                let dto = TodoDto.fromTodo todo
                connStr
                |> Sql.connect
                |> Sql.query "INSERT INTO todos (id, description, status) VALUES (@id, @desc, @status)"
                |> Sql.parameters [ "@id", Sql.uuid dto.Id; "@desc", Sql.text dto.Description; "@status", Sql.text dto.Status ]
                |> Sql.executeNonQueryAsync |> ignore
                Ok ()
            else Error "eep"
        return res
    }


    member __.UpdateStatus(todo: Todo) = async {
        let dto = TodoDto.fromTodo todo
        let flipStatus (todo: TodoDto) =
            match todo.Status with
            | "Completed" -> "Incomplete"
            | "Incomplete" -> "Completed"
        let! res =
            conn
            |> Sql.connect
            |> Sql.query "update todos set status = @status where id = @id"
            |> Sql.parameters [ "status", Sql.text (flipStatus dto) ;"id", Sql.uuid todo.Id ]
            |> Sql.executeNonQueryAsync
            |> Async.AwaitTask
        match res with
        | 1 -> return Ok ()
        | _ -> return Error "Invalid request"
    }

    member __.DeleteTodo(todo: Todo) = async {
        let! del =
            conn
            |> Sql.connect
            |> Sql.query "delete from todos where id = @id"
            |> Sql.parameters [ "id", Sql.uuid todo.Id ]
            |> Sql.executeNonQueryAsync
            |> Async.AwaitTask
        match del with
        | 1 -> return Ok ()
        | _ -> return Error "Not found"
    }


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
                  match! storage.AddTodo (todo, conn) with
                  | Ok () -> return todo
                  | Error e -> return failwith e
              }
      updateStatus =
          fun todo ->
              async {
                  match! storage.UpdateStatus todo with
                  | Ok () -> return todo
                  | Error e -> return failwith e
              }
      deleteTodo =
          fun todo ->
              async {
                  match! storage.DeleteTodo todo with
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
