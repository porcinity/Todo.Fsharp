namespace Shared

open System


type TodoStatus =
    | Incomplete
    | Completed

type Todo = { Id: Guid; Description: string; Status: TodoStatus }

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
      updateStatus: Todo -> Async<Todo>
      deleteTodo: Todo -> Async<Todo>
      deleteTodos: unit -> Async<Todo list> }

type TodoDto = { Id: Guid; Description: string; Status: string }

module TodoDto =
    let fromTodo (todo: Todo) =
        { Id = todo.Id
          Description = todo.Description
          Status =
              match todo.Status with
              | Incomplete -> "Incomplete"
              | Completed -> "Completed" }

    let ofTodo (todoDto: TodoDto) : Todo =
        { Id = todoDto.Id
          Description = todoDto.Description
          Status =
              match todoDto.Status with
              | "Incomplete" -> Incomplete
              | "Completed" -> Completed }