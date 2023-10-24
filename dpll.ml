(* MP1 2023/2024 - dpll.ml *)

open List

(* fonctions utilitaires *)
(* ----------------------------------------------------------- *)
(* filter_map : ('a -> 'b option) -> 'a list -> 'b list
   disponible depuis la version 4.08.0 de OCaml dans le module List :
   pour chaque élément de `list', appliquer `filter' :
   - si le résultat est `Some e', ajouter `e' au résultat ;
   - si le résultat est `None', ne rien ajouter au résultat.
   Attention, cette implémentation inverse l'ordre de la liste *)
let filter_map filter list =
  let rec aux list ret =
    match list with
    | [] -> ret
    | h :: t -> (
        match filter h with None -> aux t ret | Some e -> aux t (e :: ret))
  in
  aux list []

(* print_modele : int list option -> unit
   affichage du résultat *)
let print_modele : int list option -> unit = function
  | None -> print_string "UNSAT\n"
  | Some modele ->
      print_string "SAT\n";
      let modele2 = sort (fun i j -> abs i - abs j) modele in
      List.iter
        (fun i ->
          print_int i;
          print_string " ")
        modele2;
      print_string "0\n"

let rec remove_el l clause =
  match clause with
  | [] -> []
  | h :: t when h = l -> remove_el l t
  | h :: t -> h :: remove_el l t

let getListOfProp (clauses : int list list) : int list =
  let rec aux (clauses : int list list) (ens : int list) : int list =
    match clauses with
    | [] -> ens
    | clause1 :: liste2clauses ->
        aux liste2clauses
          (ens @ List.filter (fun e -> not (List.mem e ens)) clause1)
  in
  aux clauses []

let getMinLgClause clauses =
  let rec aux clauses lg =
    match clauses with
    | [] -> lg
    | h :: t -> if List.length h < lg then aux t (List.length h) else aux t lg
  in
  aux clauses (List.length (List.hd clauses))

let getMinClause clauses =
  let minlg = getMinLgClause clauses in
  let rec aux clauses minlg ens =
    match clauses with
    | [] -> ens
    | h :: t ->
        if List.length h = minlg then aux t minlg (ens @ h) else aux t minlg ens
  in
  aux clauses minlg []

let pp_clause clause =
  List.iter
    (fun i ->
      print_int i;
      print_string ";")
    clause

let pp_clauses clauses =
  List.iter
    (fun i ->
      print_string "[";
      pp_clause i;
      print_string "] ; ")
    clauses

(* ----------------------------------------------------------- *)

(* simplifie : int -> int list list -> int list list
   applique la simplification de l'ensemble des clauses en mettant
   le littéral l à vrai *)
let rec simplifie l clauses =
  match clauses with
  | [] -> []
  | clause1 :: liste2clauses when List.mem l clause1 ->
      simplifie l liste2clauses
  | clause1 :: liste2clauses when List.mem (-l) clause1 ->
      remove_el (-l) clause1 :: simplifie l liste2clauses
  | clause1 :: liste2clauses -> clause1 :: simplifie l liste2clauses

(* solveur_split : int list list -> int list -> int list option
   exemple d'utilisation de `simplifie' *)
(* cette fonction ne doit pas être modifiée, sauf si vous changez
   le type de la fonction simplifie *)
let rec solveur_split clauses interpretation =
  (* l'ensemble vide de clauses est satisfiable *)
  if clauses = [] then Some interpretation
  else if (* la clause vide n'est jamais satisfiable *)
          mem [] clauses then None
  else
    (* branchement *)
    let l = hd (hd clauses) in
    let branche = solveur_split (simplifie l clauses) (l :: interpretation) in
    match branche with
    | None -> solveur_split (simplifie (-l) clauses) (-l :: interpretation)
    | _ -> branche

(* solveur dpll récursif *)
(* ----------------------------------------------------------- *)

(* pur : int list list -> int
    - si 'clauses' contient au moins un littéral pur, retourne
      ce littéral ;
    - sinon, lève une exception `Failure "pas de littéral pur"' *)
let rec pur clauses =
  let propositionslist = getListOfProp clauses in
  let rec aux list2prop =
    match list2prop with
    | [] -> raise Not_found
    | h :: t when not (List.mem (-h) propositionslist) -> h
    | h :: t -> aux t
  in
  aux propositionslist

(* unitaire : int list list -> int
    - si 'clauses' contient au moins une clause unitaire, retourne
      le littéral de cette clause unitaire ;
    - sinon, lève une exception `Not_found' *)
let rec unitaire clauses =
  match clauses with
  | [] -> raise Not_found
  | (a :: []) :: t -> a
  | h :: t -> unitaire t

let choix (clauses : int list list) : int =
  let mlist = getMinClause clauses in
  let rec aux list2prop =
    match list2prop with
    | [] -> List.hd mlist
    | h :: t when List.mem h mlist && List.mem (-h) mlist -> h
    | h :: t -> aux t
  in
  aux mlist

(* solveur_dpll_rec : int list list -> int list -> int list option *)
let rec solveur_dpll_rec clauses interpretation =
  (* Cas 1 : La formule est satisfiable => Renvoie une interprétation qui la satisfait *)
  if clauses = [] then Some interpretation
  else if
    (* Cas 2 : La fonction aboutie sur un échec => La formule n'est pas satisfiable *)
    List.mem [] clauses
  then None
  else
    let u = try unitaire clauses with Not_found -> 0 in
    (* Cas 3 : Si on trouve un littéral unitaire => Simplification du littéral et continue *)
    if u != 0 then solveur_dpll_rec (simplifie u clauses) (u :: interpretation)
    else
      (* Cas 4 : Si on trouve un littéral pur => Simplification du littéral et continue *)
      let p = try pur clauses with Not_found -> 0 in
      if p != 0 then solveur_dpll_rec (simplifie p clauses) (p :: interpretation)
      else
        (* Cas 5 : Sinon, on choisit un littéral, et on simplifie soit par ce littéral, soit par sa négation *)
        let c = choix clauses in
        let b = solveur_dpll_rec (simplifie c clauses) (c :: interpretation) in
        match b with
        | None ->
            solveur_dpll_rec (simplifie (-c) clauses) (-c :: interpretation)
        | _ -> b

(* ----------------------------------------------------------- *)
(* ensembles de clauses de test *)

let ex =
  [
    [ 1; 2; 3 ];
    [ 1; -2; -3 ];
    [ 1; -4 ];
    [ -2; -3; -4 ];
    [ -1; -2; 3 ];
    [ 5; 6 ];
    [ 5; -6 ];
    [ 2; -5 ];
    [ -3; -5 ];
  ]

let exemple_3_12 =
  [ [ 1; 2; -3 ]; [ 2; 3 ]; [ -1; -2; 3 ]; [ -1; -3 ]; [ 1; -2 ] ]

let exemple_7_2 = [ [ 1; -1; -3 ]; [ -2; 3 ]; [ -2 ] ]

let exemple_7_4 =
  [ [ 1; 2; 3 ]; [ -1; 2; 3 ]; [ 3 ]; [ 1; -2; -3 ]; [ -1; -2; -3 ]; [ -3 ] ]

let exemple_7_8 = [ [ 1; -2; 3 ]; [ 1; -3 ]; [ 2; 3 ]; [ 1; -2 ] ]
let systeme = [ [ -1; 2 ]; [ 1; -2 ]; [ 1; -3 ]; [ 1; 2; 3 ]; [ -1; -2 ] ]

let coloriage =
  [
    [ 1; 2; 3 ];
    [ 4; 5; 6 ];
    [ 7; 8; 9 ];
    [ 10; 11; 12 ];
    [ 13; 14; 15 ];
    [ 16; 17; 18 ];
    [ 19; 20; 21 ];
    [ -1; -2 ];
    [ -1; -3 ];
    [ -2; -3 ];
    [ -4; -5 ];
    [ -4; -6 ];
    [ -5; -6 ];
    [ -7; -8 ];
    [ -7; -9 ];
    [ -8; -9 ];
    [ -10; -11 ];
    [ -10; -12 ];
    [ -11; -12 ];
    [ -13; -14 ];
    [ -13; -15 ];
    [ -14; -15 ];
    [ -16; -17 ];
    [ -16; -18 ];
    [ -17; -18 ];
    [ -19; -20 ];
    [ -19; -21 ];
    [ -20; -21 ];
    [ -1; -4 ];
    [ -2; -5 ];
    [ -3; -6 ];
    [ -1; -7 ];
    [ -2; -8 ];
    [ -3; -9 ];
    [ -4; -7 ];
    [ -5; -8 ];
    [ -6; -9 ];
    [ -4; -10 ];
    [ -5; -11 ];
    [ -6; -12 ];
    [ -7; -10 ];
    [ -8; -11 ];
    [ -9; -12 ];
    [ -7; -13 ];
    [ -8; -14 ];
    [ -9; -15 ];
    [ -7; -16 ];
    [ -8; -17 ];
    [ -9; -18 ];
    [ -10; -13 ];
    [ -11; -14 ];
    [ -12; -15 ];
    [ -13; -16 ];
    [ -14; -17 ];
    [ -15; -18 ];
  ]

(* tests *)
(* ----------------------------------------------------------- *)

(* let () =
  print_string "phi = ";
  pp_clauses ex;
  print_endline "";
  print_string "Taille de la plus petite clause = ";
  print_int (getMinLgClause ex);
  print_endline "";
  print_string "Ensemble des propositions dans les plus petites clauses : ";
  pp_clause (getMinClause ex);
  print_endline "";
  print_string "Proposition choisie par split : ";
  print_int (choix ex);
  print_endline "";
  print_string "Ensembre des propositions de phi : ";
  pp_clause (getListOfProp ex);
  print_endline "";
  print_endline "";
  print_string "DPLL phi : ";
  print_modele (solveur_dpll_rec exemple_7_4 []);
  print_endline "";
  print_int (pur ex);
  print_endline "" *)

let () =
   let clauses = Dimacs.parse Sys.argv.(1) in
   print_modele (solveur_dpll_rec clauses [])
