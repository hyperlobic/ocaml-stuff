let get_digits i = 
  let rec loop acc = function
    | 0 -> List.rev acc
    | x -> loop (x mod 10 :: acc) (x / 10)
  in
  loop [] i

let print_digits list =
  let open Printf in
  list
  |> List.rev
  |> List.iter (fun i -> printf "%d " i);
  print_newline()

let list_filteri f l = 
  let rec filter i acc = function
    | [] -> List.rev acc
    | hd :: tl ->
      if f i hd then filter (i+1) (hd :: acc) tl
      else filter (i+1) acc tl
  in
  filter 0 [] l

let luhn_chksum i =
  let digits = get_digits i in
  let even_sum = 
    digits 
    |> list_filteri (fun i n -> i mod 2 = 1)
    |> List.fold_left (fun sum x ->
      sum +
      (get_digits (x * 2)
      |> List.fold_left (fun s y -> s + y) 0)) 0 
  in
  let odd_sum = 
    digits
    |> list_filteri (fun i n -> i mod 2 = 0)
    |> List.fold_left (fun sum x -> sum + x) 0
  in
  odd_sum + even_sum

let luhn_is_valid i = 
  luhn_chksum i mod 10 == 0

let test i =
  let valid = luhn_is_valid i in
  let valid_str = match valid with
  | true -> "valid"
  | false -> "not valid"
  in
  Printf.printf "%d is %s\n" i valid_str

let () = 
  test 79927398713;
  test 82908457546;

  
  
