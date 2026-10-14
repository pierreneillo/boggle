type dictionary = (string, bool) Hashtbl.t

let n_words = 411430 (*Used only as an estimate for hash table size*)
let l_max = 18
let wordlist_fn = "ODS8.txt"
let dice_fn = "dice.txt"
let string_of_char = String.make 1

(*Swaps elements of `a` in position `i` and `j`, which are supposed to be valid indexes*)
let swap (a : 'a array) (i : int) (j : int) : unit =
  let temp = a.(j) in
  a.(j) <- a.(i);
  a.(i) <- temp

let shuffle ?(seed : int option) (a : 'a array) =
  match seed with
  | Some k -> Random.init k
  | None ->
      Random.self_init ();
      let n = Array.length a in
      for i = 0 to n - 1 do
        let j = Random.int n in
        swap a i j
      done

let read_words (fn : string) : string list =
  let rec read_aux (fd : in_channel) (acc : string list) : string list =
    try
      let w = input_line fd in
      if String.length w > 3 then read_aux fd (w :: acc) else read_aux fd acc
    with
    | End_of_file ->
        close_in fd;
        acc
    | e ->
        close_in_noerr fd;
        raise e
  in
  read_aux (open_in fn) []

let prefixes (w : string) : string list =
  (*Returns the list of prefixes of a string of length n*)
  let rec prefixes_aux (s : string) (n : int) (l : string list) : string list =
    if n = 0 then l else prefixes_aux (String.sub s 0 (n - 1)) (n - 1) (s :: l)
  in
  prefixes_aux w (String.length w) []

let is_prefix (w1 : string) (w2 : string) : bool =
  if String.length w1 <= String.length w2 then
    w1 = String.sub w2 0 (String.length w1)
  else w2 = String.sub w1 0 (String.length w2)

let is_equal (w1 : string) (w2 : string) : bool = w1 = w2

let remove_prefixes (l : string list) : string list =
  let rec remove_prefixes_aux (l : string list) : string list =
    match l with
    | [] -> []
    | x :: q ->
        if List.exists (is_prefix x) q then
          (* The word is already prefix of another in the list: bye bye*)
          remove_prefixes_aux q
        else x :: remove_prefixes_aux q
  in
  remove_prefixes_aux
    (List.sort (fun s1 s2 -> String.length s1 - String.length s2) l)

let remove_duplicates (l : string list) : string list =
  let rec remove_duplicates_aux (l : string list) : string list =
    match l with
    | [] -> []
    | x :: q ->
        if List.exists (is_equal x) q then
          (* The word is already in the list*)
          remove_duplicates_aux q
        else x :: remove_duplicates_aux q
  in
  remove_duplicates_aux
    (List.sort (fun s1 s2 -> String.length s1 - String.length s2) l)

let init_dict () : dictionary =
  let dict : (string, bool) Hashtbl.t =
    Hashtbl.create (n_words * 10)
    (* We also put prefixes in the dict, hence the x10 *)
  in
  let l = read_words wordlist_fn in
  List.iter
    (fun w ->
      prefixes w
      |> List.map (fun s -> (s, false))
      |> List.to_seq |> Hashtbl.add_seq dict)
    l;
  l |> List.map (fun x -> (x, true)) |> List.to_seq |> Hashtbl.add_seq dict;
  dict

type grid = char array array

let grid_ex : grid =
  [|
    [| 'O'; 'I'; 'T'; 'E'; 'R' |];
    [| 'E'; 'A'; 'O'; 'G'; 'R' |];
    [| 'N'; 'E'; 'N'; 'T'; 'D' |];
    [| 'D'; 'P'; 'I'; 'R'; 'H' |];
    [| 'E'; 'E'; 'A'; 'T'; 'O' |];
  |]

let read_grid (fn : string) : grid =
  let lines = read_words fn in
  Array.of_list (List.map (fun x -> Array.of_seq (String.to_seq x)) lines)

let dice : char array array =
  let lines = read_words dice_fn in
  List.map (fun x -> Array.of_seq (String.to_seq x)) lines |> Array.of_list

let gen_grid ?(seed : int option) (n : int) : grid =
  let g = Array.init n (fun x -> Array.make n ' ') in
  let shuffled_dice = Array.copy dice in
  (match seed with
  | Some s -> shuffle ~seed:s shuffled_dice
  | None -> shuffle shuffled_dice);
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      (*Dice to place*)
      let d = shuffled_dice.((i * n) + j) in
      (*We choose a face*)
      g.(i).(j) <- d.(Random.int 6)
    done
  done;
  g

let in_grid (n : int) ((x, y) : int * int) : bool =
  0 <= x && x < n && 0 <= y && y < n

let neighbours ((x, y) : int * int) (n : int) (forbidden : (int * int) list) :
    (int * int) list =
  let deltas =
    [ (-1, -1); (-1, 0); (-1, 1); (0, -1); (0, 1); (1, -1); (1, 0); (1, 1) ]
  in
  let poss =
    List.filter_map
      (fun (dx, dy) ->
        let nx = x + dx in
        let ny = y + dy in
        if in_grid n (nx, ny) then Some (nx, ny) else None)
      deltas
  in
  List.filter (fun x -> not (List.mem x forbidden)) poss

(*Returns the list of existing words in a grid (according to the `dict` dictionnary, which must have been defined with `init_dict`)*)
let find_words ?(keep_prefixes : bool option) (dict : dictionary)
    (g : grid) : string list =
  let n = Array.length g in
  let found : string list ref = ref [] in
  let rec crawl (pos : int * int) (pos_acc : (int * int) list)
      (word_acc : string) : unit =
    (*We work on the current grid point*)
    match Hashtbl.find_opt dict word_acc with
    | Some b ->
        if b then found := word_acc :: !found;
        (*Exploration continues, since the current word is a prefix of an existing word*)
        let v = neighbours pos n pos_acc in
        List.iter
          (fun (x, y) ->
            crawl (x, y) ((x, y) :: pos_acc)
              (word_acc ^ string_of_char g.(x).(y)))
          v
    | None ->
        (*The current word is not a prefix of any word, we stop there*)
        ()
  in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      crawl (i, j) [ (i, j) ] (string_of_char g.(i).(j))
    done
  done;
  match keep_prefixes with
  | None | Some false -> List.sort String.compare (remove_prefixes !found)
  | Some true -> List.sort String.compare (remove_duplicates !found)

let d = init_dict ()

let boggle_res ?(keep_prefixes: bool option)(fn : string) : unit =
  let words = match keep_prefixes with
	| Some b  -> find_words ~keep_prefixes:b d (read_grid fn)
	| None -> find_words d (read_grid fn)
  in
  Printf.printf "Game '%s': %d words\n\n" fn (List.length words);
  let r_cmp = ref (List.length words) in
  let r_i = ref 4 in
  while !r_i <= l_max && !r_cmp > 0 do
    let li =
      words
      |> List.filter (fun x -> String.length x = !r_i)
      |> List.sort String.compare
    in
    Printf.printf "%d words of %d letters, %d words of more than %d letters:\n"
      (List.length li) !r_i !r_cmp !r_i;
    r_cmp := !r_cmp - List.length li;
    li |> List.iter (fun x -> Printf.printf "\t%s\n" x);
    incr r_i
  done

let stats (n : int) (n_games : int) : unit =
  let n_words_per_size = Array.make l_max 0 in
  (*n_words.(i) is the number of words of size i+4*)
  let rec parse_words (mots : string list) : unit =
    match mots with
    | [] -> ()
    | x :: q ->
        let i = String.length x - 4 in
        n_words_per_size.(i) <- n_words_per_size.(i) + 1;
        parse_words q
  in
  for i = 0 to n_games - 1 do
    let g = gen_grid n in
    let words = find_words ~keep_prefixes:true d g in
    parse_words words
  done;
  Printf.printf "Stats for %d random games\n" n_games;
  for i = 0 to l_max - 1 do
    let mean : float =
      float_of_int n_words_per_size.(i) /. float_of_int n_games
    in
    if mean > 0. then
      Printf.printf "%.2f words with an average length of %d\n" mean (i + 4)
  done
