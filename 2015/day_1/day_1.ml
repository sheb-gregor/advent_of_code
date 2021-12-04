open Format

let get_data f = 
  let ic = open_in f in
  try
    let line = input_line ic in
    close_in ic ;
    line
  with e ->
    close_in_noerr ic;
    raise e


let step_to_inc x = 
  match x with
  | '(' -> 1
  | ')' -> -1
  | _ -> 0


let rec count_steps pos acc data_set =
  match data_set with
  | [] -> 0
  | hd :: tl ->
    let _ = if acc == -1 then printf "step: %d %d \n" pos acc else () in
    let ic = step_to_inc hd in
    let res = count_steps (pos + 1) (acc + ic) tl in
    ic + res

let () = 
  let task_input = get_data "input" in
  let parsed_input = List.init (String.length task_input) (String.get task_input) in
  let res = count_steps 0 0 parsed_input in
  printf "Result 1: %d \n" res


let () = print_endline "Day 1 Solution"
