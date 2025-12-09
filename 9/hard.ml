open Core

module Pos = struct
  type t = int * int [@@deriving sexp, compare, hash]
end

module Grid = struct
  type allowed = bool [@@deriving sexp]
  type t = allowed array array [@@deriving sexp]

  let set grid (x, y) value = grid.(x).(y) <- value
  let get grid (x, y) = try Some grid.(x).(y) with _ -> None
  let make m n = Array.make_matrix ~dimx:m ~dimy:n false

  let pp grid =
    Array.iter grid ~f:(fun row ->
        print_endline
          (Array.to_list row
          |> List.map ~f:(fun b -> if b then '#' else '.')
          |> String.of_char_list))

  let fill grid =
    let module H = Hashtbl.Make (Pos) in
    let visited = H.create () in
    let start = (0, 0) in
    let q = Queue.create () in
    Queue.enqueue q start;
    while not (Queue.is_empty q) do
      let x, y = Queue.dequeue_exn q in
      let neighbours = [ (x + 1, y); (x - 1, y); (x, y + 1); (x, y - 1) ] in
      List.iter neighbours ~f:(fun pos ->
          match get grid pos with
          | Some false ->
              if not (H.mem visited pos) then (
                H.set visited ~key:pos ~data:();
                Queue.enqueue q pos)
          | _ -> ())
    done;
    Array.iteri grid ~f:(fun x row ->
        Array.iteri row ~f:(fun y cell ->
            if Bool.(cell = false) && not (H.mem visited (x, y)) then
              set grid (x, y) true))
end

module Mapping = struct
  module H = Hashtbl.Make (Int)

  type t = { x_to_compressed : int H.t; y_to_compressed : int H.t }

  let create xs ys =
    let rec compress ?(counter = 1) h elms =
      match elms with
      | [] -> ()
      | elm :: rest ->
          H.set h ~key:elm ~data:counter;
          compress ~counter:(counter + 2) h rest
    in
    let ordered_xs = List.dedup_and_sort xs ~compare:Int.compare in
    let ordered_ys = List.dedup_and_sort ys ~compare:Int.compare in
    let x_to_compressed = H.create () in
    let y_to_compressed = H.create () in
    compress x_to_compressed ordered_xs;
    compress y_to_compressed ordered_ys;
    let dim_x =
      H.data x_to_compressed
      |> List.max_elt ~compare:Int.compare
      |> Option.value_exn
    in
    let dim_y =
      H.data y_to_compressed
      |> List.max_elt ~compare:Int.compare
      |> Option.value_exn
    in
    ({ x_to_compressed; y_to_compressed }, (dim_x + 2, dim_y + 2))

  let to_compressed t (x, y) =
    (H.find_exn t.x_to_compressed x, H.find_exn t.y_to_compressed y)
end

let rec parse_input () : Pos.t list =
  match In_channel.(input_line stdin) with
  | None -> []
  | Some line ->
      (match String.split_on_chars line ~on:[ ',' ] with
      | [ x; y ] -> (Int.of_string x, Int.of_string y)
      | _ -> failwith "Invalid input")
      :: parse_input ()

let size (x1, y1) (x2, y2) = (abs (x1 - x2) + 1) * (abs (y1 - y2) + 1)

let vertices_to_edges vertices =
  let first = List.hd vertices |> Option.value_exn ~message:"No vertices" in
  let rec go = function
    | [] -> []
    | [ last ] -> [ (last, first) ]
    | first :: second :: rest -> (first, second) :: go (second :: rest)
  in
  go vertices

let line (x1, y1) (x2, y2) =
  if x1 = x2 then
    List.range y1 y2 ~stride:(Int.compare y2 y1)
    |> List.map ~f:(fun y -> (x1, y))
  else if y1 = y2 then
    List.range x1 x2 ~stride:(Int.compare x2 x1)
    |> List.map ~f:(fun x -> (x, y1))
  else failwith "Invalid line"

let rectangle (x1, y1) (x2, y2) =
  let x = min x1 x2 in
  let y = min y1 y2 in
  let width = abs (x2 - x1) in
  let height = abs (y2 - y1) in
  (* List.cartesian_product
     (List.range x (x + width) ~stride:1)
     (List.range y (y + height) ~stride:1) *)
  (* Luckily only outline works, otherwise we would need prefix sums *)
  List.concat
    [
      line (x, y) (x + width, y);
      line (x, y) (x, y + height);
      line (x + width, y) (x + width, y + height);
      line (x, y + height) (x + width, y + height);
    ]

let () =
  let vertices = parse_input () in
  let xs = List.map vertices ~f:(fun (x, _) -> x) in
  let ys = List.map vertices ~f:(fun (_, y) -> y) in
  let mapping, (m, n) = Mapping.create xs ys in
  let grid = Grid.make m n in

  let edges = vertices_to_edges vertices in
  List.iter edges ~f:(fun (a, b) ->
      let a = Mapping.to_compressed mapping a in
      let b = Mapping.to_compressed mapping b in
      List.iter (line a b) ~f:(fun pos -> Grid.set grid pos true));

  Grid.fill grid;

  let result, _, _ =
    List.cartesian_product vertices vertices
    |> List.map ~f:(fun (a, b) -> (size a b, a, b))
    |> List.sort ~compare:(fun (a, _, _) (b, _, _) -> Int.compare b a)
    |> List.find ~f:(fun (s, a, b) ->
           let a = Mapping.to_compressed mapping a in
           let b = Mapping.to_compressed mapping b in
           List.for_all (rectangle a b) ~f:(fun pos ->
               match Grid.get grid pos with Some true -> true | _ -> false))
    |> Option.value_exn ~message:"No result"
  in
  printf !"Result: %d\n" result
