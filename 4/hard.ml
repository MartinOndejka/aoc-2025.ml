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

  let rec remove_accessible_slots ?(acc = 0) grid =
    let count =
      Array.foldi grid ~init:0 ~f:(fun x acc row ->
          Array.foldi row ~init:acc ~f:(fun y acc _ ->
              let cell = grid.(x).(y) in
              if Char.(cell = '@') && is_slot_accessible grid x y then (
                grid.(x).(y) <- '.';
                acc + 1)
              else acc))
    in
    if count = 0 then acc else remove_accessible_slots ~acc:(acc + count) grid
end

let () =
  let grid = Grid.parse () in
  let result = Grid.remove_accessible_slots grid in
  printf !"Result: %d\n" result
