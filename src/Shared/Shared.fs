namespace Shared

open System

type TodoStatus =
    | Incomplete
    | Completed

type Todo = { Id: Guid; Description: string; Status:TodoStatus }

module Todo =
    let isValid (description: string) =
        String.IsNullOrWhiteSpace description |> not

    let create (description: string) =
        { Id = Guid.NewGuid()
          Description = description
          Status = Incomplete }

module Route =
    let builder typeName methodName =
        sprintf "/api/%s/%s" typeName methodName

type ITodosApi =
    { getTodos: unit -> Async<Todo list>
      addTodo: Todo -> Async<Todo>
      deleteTodo: Todo -> Async<Todo> }
