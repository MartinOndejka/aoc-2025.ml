open Core

let min_neighbours_empty = 4

module Grid = struct
  type t = char array array [@@deriving sexp]

  let rec parse ?(grid = [||]) () =
    match In_channel.(input_line stdin) with
    | None -> grid
    | Some line ->
        let grid =
          Array.append grid [| String.to_list line |> Array.of_list |]
        in
        parse ~grid ()

  let is_slot_accessible grid x y =
    let neighbour_slots =
      [ (-1, -1); (-1, 0); (-1, 1); (0, -1); (0, 1); (1, -1); (1, 0); (1, 1) ]
    in
    List.map neighbour_slots ~f:(fun (dx, dy) ->
        try grid.(x + dx).(y + dy) with _ -> '.')
    |> List.count ~f:(fun c -> Char.(c = '.'))
    > min_neighbours_empty
end

let () =
  let grid = Grid.parse () in
  let result =
    Array.foldi grid ~init:0 ~f:(fun x acc row ->
        Array.foldi row ~init:acc ~f:(fun y acc cell ->
            if Char.(cell = '@') && Grid.is_slot_accessible grid x y then
              acc + 1
            else acc))
  in
  printf !"Result: %d\n" result
