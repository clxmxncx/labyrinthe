
(******************************************************************************)
(* voir README.md *)
(******************************************************************************)


(******************************************************************************)
(* outils *)
let wait_for_key_pressed ?(pos=(0,0)) ?(txt="(press any key)") () =
  (* https://www.oreilly.com/library/view/x-window-system/9780937175149/Chapter05.html *)
  let font = "-misc-fixed-medium-r-normal--20-200-75-75-c-100-iso8859-1" in
  Graphics.set_font font;
  Graphics.set_color Graphics.blue;
  Graphics.moveto (fst pos) (snd pos);
  Graphics.draw_string txt;
  Graphics.synchronize ();
  Graphics.read_key ()

let initialize_graphics ?(title="labyrinthes") ~size =
  Printf.printf "--- %s\n" title;
  Graphics.open_graph size;
  Graphics.set_window_title title

(* mesurer le temps d'exécution d'une fonction f *)
let time f =
  let t = Unix.gettimeofday () in
  let res = f () in
  let exec_time = Unix.gettimeofday () -. t in
  (res, exec_time)
(******************************************************************************)


(******************************************************************************)
(* taille maximum de la hauteur ou de largeur de la fenetre d'affichage *)
let size = 600
(* espace vide autour du labyrinthe: *)
let offset = 20
(* taille des parois du labyrinthe: *)
let wall_thickness = 2

(* type pour le stockage des informations concernant le dessin du labyrinthe *)
type drawing_context = {
  n: int; (* nombre de lignes *)
  p: int; (* nombre de colonnes *)
  wall_thickness: int;
  square_size: int;
  offset: int;
  width: int;
  height: int;
}

(* - n lignes, p colonnes
   - square_size est l'épaisseur d'UN mur + la longueur du côté du carré vide
   formé par l'intérieur de la case  *)
let build_drawing_context n p =
  let square_size =
    (* plus ou autant de colonnes que de lignes : *)
    if n <= p then ( size - 2*offset - wall_thickness ) / p
    (* plus de lignes que de colonnes : *)
    else ( size - 2*offset - wall_thickness ) / n
  in
  if square_size < 3 then
    let msg = Printf.sprintf
              "le nombre de ligne ou de colonnes ne doit pas depasser %d"
              ( (size - 2*offset - wall_thickness) / 3 ) in
    failwith msg
  else
  let width = wall_thickness + p*square_size + 2*offset in
  let height = wall_thickness + n*square_size + 2*offset
  in
  {
    n=n; (* n lignes *)
    p=p; (* p colonnes *)
    wall_thickness=wall_thickness;
    square_size=square_size;
    offset=offset;
    width=width;
    height=height
  }

(* type des cases du labyrinthe:
- true: le passage est libre dans la direction donnée,
- false: il y a un mur *)
type square = {
  mutable east: bool;
  mutable north: bool;
  mutable west: bool;
  mutable south: bool
}

(* 
labyrinthe à n lignes et p colonnes: 
- matrice d'élements de type square 
- maze.(i).(j): case ligne i, colonne j
*)
type labyrinthe = square array array

let init_maze n p f =
  Array.init n (fun i -> Array.init p (fun j -> f i j))

let demo_maze =
  let m = init_maze 3 4
            (fun _ _ -> {east=false; north=false; west=false; south=false}) in
  m.(0).(0).north <- true;
  m.(1).(0).south <- true;
  m.(1).(2).east <- true;
  m.(1).(3).west <- true;
  m

(* renvoie les coordonnées cartésiennes du coin bas gauche
   de la case ligne i, colonne j 
*)
let get_corner_coord dc (j, i) =
  let x_corner = dc.offset +  j * dc.square_size in
  let y_corner = dc.offset +  i * dc.square_size in
  (x_corner, y_corner)

(* dc: drawing context
   m: labyrinthe*)
let draw_maze dc m =
  (* vérification nombre de lignes *)
  assert (dc.n = (Array.length m) );
  (* vérification nombre de colonnes *)
  assert (dc.p = (Array.length m.(0)));

  Graphics.set_color Graphics.blue;
  Graphics.set_line_width dc.wall_thickness;

  (* dessin du bord du labyrinthe, mur fermé *)
  Graphics.draw_rect dc.offset dc.offset
                     (dc.p * dc.square_size ) (* width *)
                     (dc.n * dc.square_size ) (* height *);

  (* dessin des cloisons internes:
  - parcours des lignes de bas en haut
  - pour chaque ligne parcours des cases de gauche à droite
  - pour chaque case dessin du mur NORD s'il existe et du mur EST s'il existe.
  Le repère dans OCaml Graphics a son origine (0,0) en bas à gauche;
  maze.(i).(j) : ligne i colonne j*)

  Array.iteri
    (fun i line ->
      Array.iteri
        (fun j case ->
          (* dessin eventuel de la cloison NORD : *)
          if case.north = false then begin
            let (x0, y0) = get_corner_coord dc (j, i+1) in
            let x1 = x0 + dc.square_size in
            let y1 = y0 in
            Graphics.draw_segments [| (x0, y0, x1, y1) |]
            end;
          (* dessin éventuel de la cloison EST *)
          if case.east = false then begin
            let (x0, y0) = get_corner_coord dc (j+1, i) in
            let x1 = x0  in
            let y1 = y0 + dc.square_size in
            Graphics.draw_segments [| (x0, y0, x1, y1) |]
            end;
        ) line
    ) m ;
  ()

(* surligner la case ligne i colonne j *)
let highlight_square ?(fill=false) ~color dc (j, i) =
  (* dc: drawing context *)
  Graphics.set_color color;
  let (x0, y0) = get_corner_coord dc (j, i) in
  let x = x0 + dc.wall_thickness + 1 in
  let y = y0 + dc.wall_thickness + 1 in
  let w = dc.square_size - 2*dc.wall_thickness - 2 in
  let h = w in
  if fill then
    Graphics.fill_rect x y w h
  else
    Graphics.draw_rect x y w h

(* marquer un point au centre de la case ligne i colonne j) *)
(* dc: drawing context *)
let dot_square ~color dc (j, i) =
  Graphics.set_color color;
  let (x0, y0) = get_corner_coord dc (j, i) in
  let h =  dc.wall_thickness + ((dc.square_size - dc.wall_thickness)/2) in
  let w = h in
  let x = x0 + w in
  let y = y0 + h in
  Graphics.fill_circle x y  (dc.square_size / 5)

(* trace un chemin stocké dans une pile *)
(* dc: drawing context *)
let rec draw_path ~color dc s =
  if Stack.is_empty s then ()
  else
  let (j, i) = Stack.pop s in
    dot_square ~color:color dc (j, i);
    draw_path ~color dc s
(******************************************************************************)


(******************************************************************************)
(* génération de labyrinthes par exploration exhausitive 
  - [each] est appelée à chaque itération; par défaut fonction sans effet,
  utilisée par la fonction make_maze_prim_wait pour l'affichage progressif
  - n lignes, p colonnes; initialement, toutes les cases sont bordées de murs *)
(* dc: drawing context *)
let make_maze_prim ?(each=fun _ _ -> ()) dc =
  let maze = init_maze dc.n dc.p
            (fun _ _ -> {east=false; north=false; west=false; south=false}) in
  let stack = Stack.create () in
  let visited = Array.make_matrix dc.n dc.p false in
  (* on part d'une case choisie au hasard *)
  let i = Random.int dc.n in (* ligne i, au hasard, entre 0 et n-1 *)
  let j = Random.int dc.p in (* colonne j, au hasard, entre 0 et p-1 *)
  Stack.push (j, i) stack; (* (j, i): on empile les coordonnées *)
  visited.(i).(j) <- true;
  while not (Stack.is_empty stack) do
    let (j, i) = Stack.pop stack in
    each maze (j, i);
    let poss_dir = ref [] in
    if ((j < dc.p - 1) && (not visited.(i).(j+1))) then poss_dir := 'E'::!poss_dir;
    if ((i > 0) && (not visited.(i-1).(j))) then poss_dir := 'S'::!poss_dir;
    if ((j > 0) && (not visited.(i).(j-1))) then poss_dir := 'W'::!poss_dir;
    if ((i < dc.n - 1) && (not visited.(i+1).(j))) then poss_dir := 'N'::!poss_dir;
    let nb_of_poss_dir = List.length !poss_dir in
    (* s'il y a plus d'une voisine non visitée de (j, i), on remet (j, i) sur la pile: *)
    if nb_of_poss_dir > 1 then Stack.push (j, i) stack;
    (* s'il y a au moins une voisine non visitée de (j, i), on en choisit une au hasard: *)
    if nb_of_poss_dir > 0 then
      let k = Random.int nb_of_poss_dir in
      let dir = List.nth !poss_dir k in
      match dir with  (* on creuse le mur dans cette direction *)
      | 'E' ->
              maze.(i).(j).east <- true;
              maze.(i).(j+1).west <- true;
              Stack.push (j+1, i) stack;  (* on empile les coordonnées *)
              visited.(i).(j+1) <- true
      | 'N' ->
              maze.(i).(j).north <- true;
              maze.(i+1).(j).south <- true;
              Stack.push (j, i+1) stack;  (* on empile les coordonnées *)
              visited.(i+1).(j) <- true
      | 'W' ->
              maze.(i).(j).west <- true;
              maze.(i).(j-1).east <- true;
              Stack.push (j-1, i) stack;  (* on empile les coordonnées *)
              visited.(i).(j-1) <- true
      | 'S' ->
              maze.(i).(j).south <- true;
              maze.(i-1).(j).north <- true;
              Stack.push (j, i-1) stack;  (* on empile les coordonnées *)
              visited.(i-1).(j) <- true
      | _ ->
              failwith "wrong direction"
  done;
  maze

(* même fonction, avec affichage au fur et à mesure *)
(* dc: drawing context *)
let make_maze_prim_wait ?(speedup=true) ~wait dc =
  (* temps d'attente en secondes : *)
  let wait = ref wait in
  (* fonction appelée à chaque itération : *)
  let each maze (j, i) =
    Graphics.clear_graph ();
    draw_maze dc maze;
    highlight_square ~color:Graphics.red  dc (j, i);
    Graphics.synchronize ();
    ignore (Unix.system (Printf.sprintf "sleep %.3f" !wait));
    (* accélération si le temps d'attente initial demandé est grand : *)
    if speedup then if !wait > 0.1 then wait := 0.98 *. !wait
  in
  make_maze_prim ~each dc
(******************************************************************************)


(******************************************************************************)
(* DFS: Depth First Search, Parcours en profondeur du graphe 
   - start: coordonnées du départ 
   - stop: coordonnées de l'arrivée
   - m: le labyrinthe 
   - each: fonction qui par défaut ne fait rien *)
let explore_dfs ?(each=(fun _ _ -> ())) m start stop =
  let n = Array.length m in       (* nombre de lignes *)
  let p = Array.length m.(0) in   (* nombre de colonnes *)
  let visited = Array.make_matrix n p false in (* cases déjà visitées *)
  let stack = Stack.create () in  (* pile de positions; chaque position: (abs, ord)*)
  Stack.push start stack;         (* position courante: sommet de la pile*)
  visited.(snd start).(fst start) <- true;
  let found = ref None  in
  while !found = None do
    each stack visited;
    if Stack.is_empty stack then failwith "pas de chemin" ;
    (* on récupère le sommet de la pile, position courante : *)
    let ((j, i) as pos) = Stack.pop stack in
    if pos = stop then
      begin
      (* la position atteinte est la sortie du labyrinthe *)
      Stack.push pos stack; (* on la remet au sommet de la pile *)
      found := Some (stack) (* on retourne le chemin qui y mène, cad la pile *)
      end
    else
      if (m.(i).(j).east && not visited.(i).(j+1)) then
        begin
          Stack.push pos stack;
          Stack.push (j+1,i) stack;
          visited.(i).(j+1) <- true;
        end
    else
      if (m.(i).(j).west && not visited.(i).(j-1)) then
        begin
          Stack.push pos stack;
          Stack.push (j-1,i) stack;
          visited.(i).(j-1) <- true;
        end
    else
      if (m.(i).(j).north && not visited.(i+1).(j)) then
        begin
          Stack.push pos stack;
          Stack.push (j,i+1) stack;
          visited.(i+1).(j) <- true;
        end
    else
      if (m.(i).(j).south && not visited.(i-1).(j)) then
        begin
          Stack.push pos stack;
          Stack.push (j,i-1) stack;
          visited.(i-1).(j) <- true;
        end
  done;
  each stack visited; (* utilisé pour l'affichage progressif dans explore_dfs_wait*)
  match !found with
    | None -> assert false (* cas qui ne peut normalement pas se produire:
                            pas de sortie de la boucle while si !found=None *)
    | Some s -> s  (* pile contenant le chemin;
                    la position de la sortie est au sommet de la pile *)

(* parcours DFS, avec affichage au fur et à mesure 
  - dc: drawing context 
  - wait: temps d'attente en secondes *)
let explore_dfs_wait ?(speedup=true) ~wait dc m start stop =
  let wait = ref wait in
  let each stack visited =
    (* affichage du labyrinthe *)
    Graphics.clear_graph ();
    draw_maze dc m;
    (* on colore les cases déjà visitées en orange clair  *)
    Array.iteri (fun i line -> Array.iteri (fun j case_visited ->
      if case_visited then
        highlight_square ~fill:true ~color:(Graphics.rgb 247 221 176)
                         dc (j, i)
    ) line ) visited;
    (* départ et arrivée *)
    highlight_square ~fill:true ~color:(Graphics.rgb 240 242 141) dc start;
    highlight_square ~fill:true ~color:(Graphics.rgb 79 227 207) dc stop;
    (* visualisation du chemin parcouru jusqu'ici *)
    if not (Stack.is_empty stack) then
    begin
      let s = Stack.copy stack in
      (* extrémité atteinte *)
      let h = Stack.pop s in
      dot_square ~color:(Graphics.rgb 124 88 214) dc h;
      (* et reste du chemin *)
      draw_path ~color:(Graphics.rgb 25 179 71) dc s;
      Graphics.synchronize ();
      ignore (Unix.system (Printf.sprintf "sleep %.3f" !wait));
    end;
    (* accélération si le temps d'attente initial demandé est grand : *)
    if speedup then if !wait > 0.1 then wait := 0.98 *. !wait
  in
  explore_dfs ~each m start stop
(******************************************************************************)


(******************************************************************************)
(* démos *)
let demo_some_drawings () =
  initialize_graphics ~title:"rectangle bleu"
                      ~size:(Printf.sprintf " %dx%d" 500 400);
  Graphics.set_color Graphics.blue;
  Graphics.set_line_width 3;
  Graphics.draw_rect 50 50 100 200;
  ignore (wait_for_key_pressed ())

let demo_draw_maze () =
  Graphics.open_graph " 600x600";
  let dc = build_drawing_context 3 4 in
  draw_maze dc demo_maze;
  ignore ( Graphics.read_key ())

let demo_make_maze_prim ?(speedup=true) ?(wait=0.0) n p =
  let dc = build_drawing_context n p in (* drawing context *)
  initialize_graphics ~title:"maze generation, Prim"
                      ~size:(Printf.sprintf " %dx%d" dc.width dc.height);
  Graphics.auto_synchronize false;
  let make =
    if wait=0.0 then make_maze_prim ~each:(fun _ _ -> ())
                else make_maze_prim_wait ~speedup:speedup ~wait:wait
  in
  let m = make dc in
  draw_maze dc m;
  Graphics.synchronize ();
  ignore (wait_for_key_pressed ())

let demo_dfs ?(speedup=true) ?(wait=0.0) n p =
  let dc = build_drawing_context n p in (* drawing context *)
  initialize_graphics ~title:"DFS exploration"
                      ~size:(Printf.sprintf " %dx%d" dc.width dc.height);
  Graphics.auto_synchronize false;
  let start = (0,0) in  (* (première colonne, première ligne)*)
  let stop = (dc.p-1, dc.n-1) in (* (dernière colonne, dernière ligne) *)
  let m = make_maze_prim dc in
  draw_maze dc m ;
  ignore (wait_for_key_pressed ());
  let explore =
    if wait=0.0 then explore_dfs ~each:(fun _ _ -> ())
                else explore_dfs_wait ~wait:wait ~speedup:speedup dc
  in
  let s = explore m start stop in
  draw_path ~color:Graphics.green dc s;
  ignore (wait_for_key_pressed ())
(******************************************************************************)


(******************************************************************************)
(* menu principal *)
let () =
  Random.self_init ();
  let nb_of_args = Array.length Sys.argv in
  let demo_nb = if nb_of_args = 2 then int_of_string Sys.argv.(1) else 0 in
  match demo_nb with
  | 0 -> Printf.printf "\n---> Hello world !\n\n"
  | 1 -> demo_some_drawings ()
  | 2 -> demo_draw_maze ()
  | 3 -> demo_make_maze_prim 5 7
  | 4 -> demo_make_maze_prim ~wait:0.05 5 7
  | 5 -> demo_dfs 10 14
  | 6 -> demo_dfs ~wait:0.1 10 14
  | _ -> ()
