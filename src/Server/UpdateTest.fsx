type TodoStatus =
    | Incomplete
    | Completed

type Todo = { Id: int; Title: string; Description: string; Status: TodoStatus }

let myList = [
    { Id = 1; Title = "First"; Description = "The first one!"; Status = Incomplete }
    { Id = 2; Title = "Second"; Description = "The second one!"; Status = Incomplete }
    { Id = 3; Title = "Third"; Description = "The third one!"; Status = Incomplete }
]

let updateOne lst id title =
    lst
    |> List.map (fun x ->
        if x.Id = id then { x with Title = title } else x)

let updateStatus lst id status =
    match status with
    | "done" ->
        lst
        |> List.map (fun x ->
            if x.Id = id then { x with Status = Completed } else x)
    | _ -> lst

let cycleStatus lst id =
    lst
    |> List.map (fun x ->
        match x.Id = id with
        | true ->
            match x.Status with
            | Incomplete -> { x with Status = Completed }
            | Completed -> { x with Status = Incomplete }
        | false -> x)

let cycleStatus' id =
    myList
    |> List.map (fun x ->
        match x.Id = id with
        | true ->
            match x.Status with
            | Incomplete -> { x with Status = Completed }
            | Completed -> { x with Status = Incomplete }
        | false -> x)