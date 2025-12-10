open Core

module Button = struct
  type t = int list [@@deriving sexp]

  let rec parse_buttons = function
    | [] -> ([], [])
    | str :: rest ->
        if String.is_prefix str ~prefix:"(" then
          let button : t =
            String.filter str ~f:(fun c -> Char.(c <> '(' && c <> ')'))
            |> String.split ~on:',' |> List.map ~f:Int.of_string
          in
          let buttons, rest = parse_buttons rest in
          (button :: buttons, rest)
        else ([], str :: rest)
end

let hash_fold_array a b arr = hash_fold_list a b (Array.to_list arr)

module Light = struct
  type t = bool array [@@deriving sexp, equal, compare, hash]

  let make ~len = Array.create ~len false
  let copy l = Array.copy l

  let apply_button light button =
    List.iter button ~f:(fun button -> light.(button) <- not light.(button))

  let parse line =
    let open Char in
    String.filter line ~f:(fun c -> c <> '[' && c <> ']')
    |> String.to_list
    |> List.map ~f:(fun c -> c = '#')
    |> Array.of_list
end

type problem = Light.t * Button.t list

let rec parse_input () : problem list =
  match In_channel.(input_line stdin) with
  | None -> []
  | Some line -> (
      match String.split line ~on:' ' with
      | light :: rest ->
          let light = Light.parse light in
          let buttons, _joltage = Button.parse_buttons rest in
          (light, buttons) :: parse_input ()
      | _ -> failwith "Invalid input")

let solve ((target_light, buttons) : problem) =
  let len = Array.length target_light in
  let module H = Hashtbl.Make (Light) in
  let visited = H.create () in
  let q = Queue.create () in
  let start = (Light.make ~len, 0) in
  H.set visited ~key:(fst start) ~data:();
  Queue.enqueue q start;
  let result = ref None in
  while (not (Queue.is_empty q)) && Option.is_none !result do
    let light, count = Queue.dequeue_exn q in
    let count = count + 1 in
    let neighbours =
      List.map buttons ~f:(fun button ->
          let light = Light.copy light in
          Light.apply_button light button;
          (light, count))
    in
    List.iter neighbours ~f:(fun n ->
        if Light.equal (fst n) target_light then result := Some count
        else if not (H.mem visited (fst n)) then (
          H.set visited ~key:(fst n) ~data:();
          Queue.enqueue q n))
  done;
  Option.value_exn !result ~message:"No solution found"

let () =
  let input = parse_input () in
  let result = List.map input ~f:solve |> List.fold ~init:0 ~f:( + ) in
  printf !"Result: %d\n" result
