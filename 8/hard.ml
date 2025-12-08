open Core

type pos = int * int * int [@@deriving sexp]

let rec parse_input () : pos list =
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

module Int3H = Hashtbl.Make (struct
  type t = int * int * int [@@deriving compare, sexp, hash]
end)

let rec pair_vertices ~target_size h ?(counter = 0) = function
  | [] -> failwith "Unreachable"
  | (_d, a, b) :: rest -> (
      let check () =
        Int3H.length h = target_size && Int3H.for_all h ~f:(fun x -> x = 1)
      in
      match (Int3H.find h a, Int3H.find h b) with
      | Some a_circuit, Some b_circuit ->
          Int3H.map_inplace h ~f:(fun x ->
              if x = max a_circuit b_circuit then min a_circuit b_circuit else x);
          if check () then fst3 a * fst3 b
          else pair_vertices ~target_size h ~counter rest
      | None, None ->
          let counter = counter + 1 in
          Int3H.set h ~key:a ~data:counter;
          Int3H.set h ~key:b ~data:counter;
          if check () then fst3 a * fst3 b
          else pair_vertices ~target_size h ~counter rest
      | Some circuit, None | None, Some circuit ->
          Int3H.set h ~key:a ~data:circuit;
          Int3H.set h ~key:b ~data:circuit;
          if check () then fst3 a * fst3 b
          else pair_vertices ~target_size h ~counter rest)

let () =
  let vertices = parse_input () in
  let edges =
    List.cartesian_product vertices vertices
    |> List.map ~f:(fun (a, b) -> (distance a b, a, b))
    |> List.filter ~f:(fun (d, _, _) -> Float.(d <> 0.))
    |> List.dedup_and_sort ~compare:(fun (d1, _, _) (d2, _, _) ->
           Float.compare d1 d2)
  in
  let h = Int3H.create () in
  let result = pair_vertices ~target_size:(List.length vertices) h edges in
  printf !"Result: %d\n" result
