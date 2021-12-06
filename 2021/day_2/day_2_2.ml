open Format

let get_data f =
  let ic = open_in f in
  try
    let raw_data = really_input_string ic (in_channel_length ic) in
    let raw_lines = String.split_on_char '\n' raw_data in
    let lines = List.filter (fun s -> String.length s >= 1 ) raw_lines in
    close_in ic;
    lines

  with e ->
    close_in_noerr ic;
    raise e

type command = {
  d: string;
  units: int;
}

type position = {
  x: int;
  depth: int;
  aim: int;
}

let command_of_string s =
  let chunks = String.split_on_char ' ' s in
  {
    d = List.nth chunks 0;
    units = List.nth chunks 1 |> int_of_string;
  }

let apply_cmd pos cmd = match cmd.d with
  | "forward" ->  { x = (pos.x + cmd.units); depth = (pos.depth + pos.aim*cmd.units);  aim = pos.aim }
  | "down"    ->  { x = pos.x;               depth = pos.depth; aim = (pos.aim + cmd.units) }
  | "up"      ->  { x = pos.x;               depth = pos.depth; aim = (pos.aim - cmd.units) }
  | _         ->  pos

let () =
  let data = get_data "input" in
  let res = List.fold_left (fun pos el ->  el |> command_of_string |> apply_cmd pos) {x=0;depth=0;aim=0} data in
  printf "result_1=%d \n" (res.x*res.depth);
  print_endline "Hello, World!"
