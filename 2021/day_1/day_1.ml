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

let part_1 acc el =
  let (prev, inc_count) =acc in
  let depth = int_of_string el in
  if prev = 0 then
    (depth, 0)
  else
    (if depth > prev then
       (depth, inc_count+1)
     else
       (depth, inc_count)
    )

let part_2 acc el =
  let depth = int_of_string el in
  let (f, s, t, prev_sum, inc_count) = acc in
  let _ = printf "f=%d s=%d t=%d sum=%d ic=%d\n" f, s, t, prev_sum, inc_count in
  match (f, s, t) with
  | (0, 0, 0) ->  (depth, 0,     0,     prev_sum, inc_count )
  | (_, 0, 0) ->  (f,     depth, 0,     prev_sum, inc_count)
  | (_, _, 0) ->  (f,     s,     depth, prev_sum, inc_count)
  | (_, _, _) ->
    let sum = f+s+t in
    let ic = (if prev_sum = 0
              then 0
              else ( if sum > prev_sum
                     then inc_count+1
                     else inc_count )) in
    (s, t, depth, sum, ic)

let () =
  let data = get_data "input" in
  let (_, count_1) = List.fold_left part_1 (0, 0) data in
  let (_, _, _, _, count_2) = List.fold_left part_2 (0,0,0,0,0) data in
  printf "result_1=%d \nresult_2=%d \n" count_1 (count_2+1)
