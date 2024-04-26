type dictionnaire = (string, bool) Hashtbl.t

let nb_mots = 386625
let liste_fn = "ODS6.txt"
let string_of_char = String.make 1

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
  (*Renvoie la liste des préfixes d'une chaîne de caractères de longueur n*)
  let rec prefixes_aux (s : string) (n : int) (l : string list) : string list =
    if n = 0 then l else prefixes_aux (String.sub s 0 (n - 1)) (n - 1) (s :: l)
  in
  prefixes_aux w (String.length w) []

let est_prefixe (w1 : string) (w2 : string) : bool =
  if String.length w1 <= String.length w2 then
    w1 = String.sub w2 0 (String.length w1)
  else w2 = String.sub w1 0 (String.length w2)

let suppr_prefixes (l : string list) : string list =
  let rec suppr_prefixes_aux (l : string list) : string list =
    match l with
    | [] -> []
    | x :: q ->
        if List.exists (est_prefixe x) q then
          (* Le mot est préfixe d'un autre dans la liste: on l'enlève*)
          suppr_prefixes_aux q
        else x :: suppr_prefixes_aux q
  in
  suppr_prefixes_aux
    (List.sort (fun s1 s2 -> String.length s1 - String.length s2) l)

let init_dict () : dictionnaire =
  let dico : (string, bool) Hashtbl.t =
    Hashtbl.create (nb_mots * 10)
    (*On met aussi les préfixes dans le dictionnaire, d'où le *10 *)
  in
  let l = read_words liste_fn in
  List.iter
    (fun w ->
      prefixes w
      |> List.map (fun s -> (s, false))
      |> List.to_seq |> Hashtbl.add_seq dico)
    l;
  l |> List.map (fun x -> (x, true)) |> List.to_seq |> Hashtbl.add_seq dico;
  dico

type grille = char array array

let grille_ex : grille =
  [|
    [| 'O'; 'I'; 'T'; 'E'; 'R' |];
    [| 'E'; 'A'; 'O'; 'G'; 'R' |];
    [| 'N'; 'E'; 'N'; 'T'; 'D' |];
    [| 'D'; 'P'; 'I'; 'R'; 'H' |];
    [| 'E'; 'E'; 'A'; 'T'; 'O' |];
  |]

let read_grille (fn : string) : grille =
  let lignes = read_words fn in
  Array.of_list (List.map (fun x -> Array.of_seq (String.to_seq x)) lignes)

let in_grid (n : int) ((x, y) : int * int) : bool =
  0 <= x && x < n && 0 <= y && y < n

let neighbors ((x, y) : int * int) (n : int) (forbidden : (int * int) list) :
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

(*Renvoie la liste des mots existant dans la grille (selon le dictionnaire dico , qui doit avoir été défini avec ini_dico)*)
let find_words (dico : dictionnaire) (g : grille) : string list =
  let n = Array.length g in
  let trouves : string list ref = ref [] in
  let rec crawl (pos : int * int) (pos_acc : (int * int) list)
      (word_acc : string) : unit =
    (*On traite le point actuel*)
    match Hashtbl.find_opt dico word_acc with
    | Some b ->
        if b then trouves := word_acc :: !trouves;
        (*On continue d'explorer puisque le mot actuel est un préfixe de mot existant*)
        let v = neighbors pos n pos_acc in
        List.iter
          (fun (x, y) ->
            crawl (x, y) ((x, y) :: pos_acc)
              (word_acc ^ string_of_char g.(x).(y)))
          v
    | None ->
        (*Le mot actuel n'est pas un préfixe de mot existant: on arrête la recherche*)
        ()
  in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      crawl (i, j) [ (i, j) ] (string_of_char g.(i).(j))
    done
  done;
  List.sort String.compare (suppr_prefixes !trouves)

let d = init_dict ()
let boggle fn = find_words d (read_grille fn)
