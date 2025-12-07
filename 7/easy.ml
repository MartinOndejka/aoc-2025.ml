open Core

module Grid = struct
  type t = char array array [@@deriving sexp]

  let pp grid =
    Array.iter grid ~f:(fun row ->
        print_endline (Array.to_list row |> String.of_char_list))

  let get grid (x, y) = try Some grid.(x).(y) with _ -> None
  let set grid (x, y) value = grid.(x).(y) <- value

  let rec parse ?(grid = [||]) () =
    match In_channel.(input_line stdin) with
    | None -> grid
    | Some line ->
        let grid = Array.append grid [| String.to_array line |] in
        parse ~grid ()

  let find_S grid =
    Array.foldi grid ~init:None ~f:(fun x acc row ->
        Array.foldi row ~init:acc ~f:(fun y acc cell ->
            if Char.(cell = 'S') then Some (x, y) else acc))
    |> Option.value_exn ~message:"S not found"

  let rec dfs ?(acc = 0) grid (x, y) =
    let down = (x + 1, y) in
    match get grid down with
    | Some '.' | Some '|' ->
        set grid down '|';
        dfs ~acc grid down
    | Some '^' ->
        let left = (x + 1, y - 1) in
        let right = (x + 1, y + 1) in
        set grid down '-';
        set grid left '|';
        set grid right '|';
        dfs ~acc grid left + dfs ~acc grid right + 1
    | Some '-' -> 0
    | None -> 0
    | _ -> failwith "Invalid grid"
end

let () =
  let grid = Grid.parse () in
  let start = Grid.find_S grid in
  let result = Grid.dfs grid start in
  Grid.pp grid;
  printf !"Result: %d\n" result
