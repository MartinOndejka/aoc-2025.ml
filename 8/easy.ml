open Core

module Pos = struct
  type t = int * int * int [@@deriving sexp, compare, hash]
end

module PosHTbl = Hashtbl.Make (Pos)
module IntHTbl = Hashtbl.Make (Int)

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

let rec pair_vertices h ?(counter = 0) = function
  | [] -> ()
  | (_d, a, b) :: rest -> (
      match (PosHTbl.find h a, PosHTbl.find h b) with
      | Some a_circuit, Some b_circuit ->
          PosHTbl.map_inplace h ~f:(fun x ->
              if x = max a_circuit b_circuit then min a_circuit b_circuit else x);
          pair_vertices h ~counter rest
      | None, None ->
          let counter = counter + 1 in
          PosHTbl.set h ~key:a ~data:counter;
          PosHTbl.set h ~key:b ~data:counter;
          pair_vertices h ~counter rest
      | Some circuit, None | None, Some circuit ->
          PosHTbl.set h ~key:a ~data:circuit;
          PosHTbl.set h ~key:b ~data:circuit;
          pair_vertices h ~counter rest)

let () =
  let vertices = parse_input () in
  let edges =
    List.cartesian_product vertices vertices
    |> List.map ~f:(fun (a, b) -> (distance a b, a, b))
    |> List.filter ~f:(fun (d, _, _) -> Float.(d <> 0.))
    |> List.dedup_and_sort ~compare:(fun (d1, _, _) (d2, _, _) ->
           Float.compare d1 d2)
    |> Fn.flip List.take 1000
  in
  let h = PosHTbl.create () in
  pair_vertices h edges;

  let counts = IntHTbl.create () in
  let () =
    PosHTbl.iter h ~f:(fun circuit ->
        IntHTbl.change counts circuit ~f:(function
          | None -> Some 1
          | Some count -> Some (count + 1)))
  in
  let result =
    IntHTbl.to_alist counts
    |> List.sort ~compare:(fun (_, a) (_, b) -> Int.compare b a)
    |> Fn.flip List.take 3
    |> List.fold ~init:1 ~f:(fun acc (_, count) -> acc * count)
  in
  printf !"Result: %d\n" result
