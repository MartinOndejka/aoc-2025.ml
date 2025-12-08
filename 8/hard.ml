open Core

module Pos = struct
  type t = int * int * int [@@deriving sexp, compare, hash]
end

module PosHTbl = Hashtbl.Make (Pos)

let rec parse_input () : Pos.t list =
  match In_channel.(input_line stdin) with
  | None -> []
  | Some line ->
      (match String.split_on_chars line ~on:[ ',' ] with
      | [ x; y; z ] -> (Int.of_string x, Int.of_string y, Int.of_string z)
      | _ -> failwith "Invalid input")
      :: parse_input ()

let distance (x1, y1, z1) (x2, y2, z2) =
  (Float.of_int (x1 - x2) ** 2.0)
  +. (Float.of_int (y1 - y2) ** 2.0)
  +. (Float.of_int (z1 - z2) ** 2.0)

let merge_circuits h a b =
  PosHTbl.map_inplace h ~f:(fun x -> if x = max a b then min a b else x)

let rec min_spanning_tree ~target_size h ?(counter = 0) = function
  | [] -> failwith "Unreachable"
  | (a, b) :: rest -> (
      let done_or_next counter rest =
        if
          PosHTbl.length h = target_size
          && PosHTbl.for_all h ~f:(fun x -> x = 1)
        then fst3 a * fst3 b
        else min_spanning_tree ~target_size h ~counter rest
      in
      match (PosHTbl.find h a, PosHTbl.find h b) with
      | Some a_circuit, Some b_circuit ->
          merge_circuits h a_circuit b_circuit;
          done_or_next counter rest
      | None, None ->
          let counter = counter + 1 in
          PosHTbl.set h ~key:a ~data:counter;
          PosHTbl.set h ~key:b ~data:counter;
          done_or_next counter rest
      | Some circuit, None | None, Some circuit ->
          PosHTbl.set h ~key:a ~data:circuit;
          PosHTbl.set h ~key:b ~data:circuit;
          done_or_next counter rest)

let () =
  let vertices = parse_input () in
  let edges =
    List.cartesian_product vertices vertices
    |> List.map ~f:(fun (a, b) -> (distance a b, a, b))
    |> List.filter ~f:(fun (d, _, _) -> Float.(d <> 0.))
    |> List.dedup_and_sort ~compare:(fun (d1, _, _) (d2, _, _) ->
           Float.compare d1 d2)
    |> List.map ~f:(fun (_d, a, b) -> (a, b))
  in
  let result =
    min_spanning_tree ~target_size:(List.length vertices) (PosHTbl.create ())
      edges
  in
  printf !"Result: %d\n" result
