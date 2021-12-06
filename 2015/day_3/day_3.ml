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

type coordinate = {
  x: int;
  y: int;
}

let char_list_of_string s = List.init (String.length s) (String.get s)

let do_move prev step =
  match step with
  | '>' -> { x = prev.x + 1; y=prev.y }
  | '<' -> { x = prev.x - 1; y=prev.y }
  | '^' -> { x = prev.x; y=prev.y + 1 }
  | 'v' -> { x = prev.x; y=prev.y - 1 }
  | _   -> { x = prev.x; y=prev.y }

let houses  = Hashtbl.create 1024

let visit_to_house pos =
  match Hashtbl.find_opt houses pos with
  | Some visits -> Hashtbl.add houses pos (visits+1) ; visits+1
  | None -> Hashtbl.add houses pos 1 ; 1

let visit_next prev_state step =
  let (prev, house_count) = prev_state in
  let next = do_move prev step in
  let v = visit_to_house next in
  if v > 1 then (next, house_count) else (next, house_count+1)

let part_1 steps =
  let _ =  visit_to_house {x=0;y=0} in
  let (_, v) = List.fold_left visit_next ({ x=0; y=0 }, 1) steps in
  printf "houses=%d \n" v


let visit_next_2 prev_state step =
  let (santa_prev, robo_prev, house_count, actor) = prev_state in
  match actor with
  | "santa" ->
    let (next, count) = visit_next (santa_prev, house_count) step in
    (next, robo_prev, count, "robo")
  | "robo" ->
    let (next, count) = visit_next (robo_prev, house_count) step in
    (santa_prev, next, count, "santa")
  |  _ -> prev_state

let part_2 steps =
  let _ = visit_to_house {x=0;y=0} in
  let _ = visit_to_house {x=0;y=0} in
  let init = ({x=0;y=0}, {x=0;y=0}, 1, "santa") in
  let (_,_, v, _) = List.fold_left visit_next_2 init steps in
  printf "houses=%d \n" v

let () =
  let data = get_data "input" in
  let steps = char_list_of_string data in
  part_2 steps
