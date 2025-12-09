open Core

module Pos = struct
  type t = int * int [@@deriving sexp, compare, hash]
end

let rec parse_input () : Pos.t list =
  match In_channel.(input_line stdin) with
  | None -> []
  | Some line ->
      (match String.split_on_chars line ~on:[ ',' ] with
      | [ x; y ] -> (Int.of_string x, Int.of_string y)
      | _ -> failwith "Invalid input")
      :: parse_input ()

let volume (x1, y1) (x2, y2) = (abs (x1 - x2) + 1) * (abs (y1 - y2) + 1)

let () =
  let input = parse_input () in
  let result =
    List.cartesian_product input input
    |> List.map ~f:(fun (a, b) -> volume a b)
    |> List.max_elt ~compare:Int.compare
    |> Option.value_exn ~message:"No result"
  in
  printf !"Result: %d\n" result
