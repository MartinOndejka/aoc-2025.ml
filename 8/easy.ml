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

module IntT3H = Hashtbl.Make (struct
  type t = int * int * int [@@deriving compare, sexp, hash]
end)

module IntH = Hashtbl.Make (Int)

let rec pair_vertices h ?(counter = 0) = function
  | [] -> ()
  | (_d, a, b) :: rest -> (
      match (IntT3H.find h a, IntT3H.find h b) with
      | Some a_circuit, Some b_circuit ->
          IntT3H.map_inplace h ~f:(fun x ->
              if x = max a_circuit b_circuit then min a_circuit b_circuit else x);
          pair_vertices h ~counter rest
      | None, None ->
          let counter = counter + 1 in
          IntT3H.set h ~key:a ~data:counter;
          IntT3H.set h ~key:b ~data:counter;
          pair_vertices h ~counter rest
      | Some circuit, None | None, Some circuit ->
          IntT3H.set h ~key:a ~data:circuit;
          IntT3H.set h ~key:b ~data:circuit;
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
  let h = IntT3H.create () in
  pair_vertices h edges;

  let counts = IntH.create () in
  let () =
    IntT3H.iter h ~f:(fun circuit ->
        IntH.change counts circuit ~f:(function
          | None -> Some 1
          | Some count -> Some (count + 1)))
  in
  let result =
    IntH.to_alist counts
    |> List.sort ~compare:(fun (_, a) (_, b) -> Int.compare b a)
    |> Fn.flip List.take 3
    |> List.fold ~init:1 ~f:(fun acc (_, count) -> acc * count)
  in
  printf !"Result: %d\n" result
