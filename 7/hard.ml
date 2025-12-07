open Core

module Matrix = struct
  type 'a t = 'a array array [@@deriving sexp]

  let get m (x, y) = try Some m.(x).(y) with _ -> None
  let get_exn m (x, y) = m.(x).(y)
  let set m (x, y) value = m.(x).(y) <- value
  let init rows cols ~f = Array.init rows ~f:(fun _ -> Array.init cols ~f)
end

let rec parse ?(grid = [||]) () =
  match In_channel.(input_line stdin) with
  | None -> grid
  | Some line ->
      let grid = Array.append grid [| String.to_array line |] in
      parse ~grid ()

let d_of_grid grid =
  Matrix.init (Array.length grid) (Array.length grid.(0)) ~f:(fun _ -> 0)

let find_S grid =
  Array.foldi grid ~init:None ~f:(fun x acc row ->
      Array.foldi row ~init:acc ~f:(fun y acc cell ->
          if Char.(cell = 'S') then Some (x, y) else acc))
  |> Option.value_exn ~message:"S not found"

let rec dfs grid d (x, y) =
  let down = (x + 1, y) in
  match Matrix.get grid down with
  | Some '.' -> dfs grid d down
  | Some '^' ->
      let left = (x + 1, y - 1) in
      let right = (x + 1, y + 1) in
      if Matrix.get_exn d down = 0 then
        Matrix.set d down (dfs grid d left + dfs grid d right);
      Matrix.get_exn d down
  | None -> 1
  | _ -> failwith "Invalid grid"

let () =
  let grid = parse () in
  let start = find_S grid in
  let d = d_of_grid grid in
  let result = dfs grid d start in
  printf !"Result: %d\n" result
