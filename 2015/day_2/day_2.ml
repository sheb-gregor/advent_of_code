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

type prism = {
  length: int;
  width: int;
  height: int;
}

let prism_of_string s =
  let parts = String.split_on_char 'x' s in
  {
    length = List.nth parts 0 |> int_of_string;
    width = List.nth  parts 1 |> int_of_string;
    height = List.nth parts 2 |> int_of_string
  }

let prism_surface_area p =
  let side_1 = p.length *p.width in
  let side_2 = p.width*p.height in
  let side_3 = p.height*p.length in
  let extra = if side_1 > side_2 then
      (if side_2 > side_3 then side_3 else side_2 )
    else
      (if side_1 > side_3 then side_3 else side_1 ) in
  (2*side_1 + 2*side_2 + 2*side_3 , extra )

let part_1 acc line =
  let (area,extra) = line |>  prism_of_string |> prism_surface_area in
  acc + area + extra

let () =
  let data = get_data "input" in
  let total = List.fold_left part_1 0 data in
  printf "total_surface=%d \n" total

let perimetr x y = 2*x+2*y

let smallest_perimeter p =
  let side_1 = perimetr p.length p.width in
  let side_2 = perimetr p.width p.height in
  let side_3 = perimetr p.height p.length in
  if side_1 > side_2 then
    (if side_2 > side_3 then side_3 else side_2 )
  else
    (if side_1 > side_3 then side_3 else side_1 )

let prism_volume p = p.length * p.width * p.height

let part_2 acc line =
  let p = prism_of_string line in
  let sp = smallest_perimeter p in
  let v = prism_volume p in
  acc + sp + v

let () =
  let data = get_data "input" in
  let total = List.fold_left part_2 0 data in
  printf "feet_of_ribbon=%d \n" total
