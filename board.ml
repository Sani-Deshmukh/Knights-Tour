open Storage

(*
 * Helper functions.
 *)

(* Return `true` if a loc is valid on a board of size
 * `nrows` rows by `ncols` columns. *)
let ok_loc nrows ncols (row, col) =
  row >= 0 && row < nrows && col >= 0 && col < ncols

(* Raise an `Invalid_argument` exception due to a bad location.
 * `name` is the name of the function, and `cause` is the
 * reason for the argument being bad. *)
let bad_loc name cause (row, col) =
  let msg = 
    Printf.sprintf "%s: %s;  row = %d col = %d%!" 
      name cause row col
  in
    invalid_arg msg


(*
 * The board module type and module.  
 * It represents the state of the knight's tour solution.
 *)

module type Board =
  sig
    type loc = Loc.t
    type t

    val make                    : int -> int -> t
    val get_dimensions          : t -> int * int
    val get_last                : t -> loc
    val get_index               : t -> loc -> int option
    val get_reachable           : t -> loc -> int option
    val get_loc_counts_from_loc : t -> loc -> (loc * int) list
    val place                   : t -> loc -> t
    val undo                    : t -> t
    val is_solved               : t -> bool
    val get_placed              : t -> loc list
  end

module Make (S: Storage) : Board =
  struct
    type loc = Loc.t

    type t = 
      {
        nrows      : int;
        ncols      : int;
        size       : int;       (* total # of squares on board *)
        placed     : loc list;  (* locations of all knights placed on board *)
        last_index : int;       (* index of last knight placed *)
        indices    : S.t;
        reachable  : S.t
      }

    (* Helper functions. *)

    let check_bounds board loc = 
      ok_loc board.nrows board.ncols loc

    (* Part B *)
    (* B.1 *)
    let init_reachable nrows ncols =
      let storage = S.make nrows ncols in 
      let knight_moves = [(-2,-1); (-2,1); (2,-1); (2,1);
                      (-1,-2); (-1,2); (1,-2); (1,2)] in
      let count_moves (row, col) = 
        List.fold_left (fun acc (dr, dc) -> 
          let r, c = row + dr, col + dc in 
          if r >=0 && r < nrows && c >= 0 && c < ncols then acc + 1 
          else acc) 0 knight_moves

        in 
        let rec update_storage row col storage = 
          if row >= nrows then storage 
          else if col >= ncols then update_storage (row + 1) 0 storage
          else 
            let count = count_moves(row, col) in 
            let updated_storage = S.set storage (row, col) count in 
            update_storage row (col + 1) updated_storage in 
        update_storage 0 0 storage



    (* Interface functions. *)

    let make nrows ncols = 
      {
        nrows      = nrows;
        ncols      = ncols;
        size       = nrows * ncols;
        placed     = [];
        last_index = 0;
        indices    = S.make nrows ncols;
        reachable  = init_reachable nrows ncols
      }

    let get_dimensions board =
      (board.nrows, board.ncols)

    let get_last board =
      match board.placed with
        | [] -> raise Not_found
        | h :: _ -> h

    let get_index board loc = 
      if not (check_bounds board loc) then
        bad_loc "get_index" "location off board" loc
      else
        S.get board.indices loc

    let get_reachable board loc = 
      if not (check_bounds board loc) then
        bad_loc "get_reachable" "location off board" loc
      else
        S.get board.reachable loc

    (* B. 2*)
  let get_loc_counts_from_loc board loc =
      if not (check_bounds board loc) then
        invalid_arg (Printf.sprintf "Invalid location");

      let knight_moves = [(-2,-1); (-2,1); (2,-1); (2,1);
                          (-1,-2); (-1,2); (1,-2); (1,2)] in
      let (row, col) = loc in
      let valid_moves =
        List.filter_map (fun (dr, dc) ->
          let r, c = row + dr, col + dc in
          if check_bounds board (r, c) && S.get board.indices (r, c) = None then
            match S.get board.reachable (r, c) with
            | Some count -> Some ((r, c), count)
            | None -> None
          else None
        ) knight_moves
      in
      valid_moves


  (* B.3  *)
  let place board loc = 
    let (row, col) = loc in 
    if not (check_bounds board (row, col)) then
      invalid_arg ("Invalid location");
  
    if S.get board.indices (row, col) <> None then
      invalid_arg ("Invalid location");
  
    let new_index = board.last_index + 1 in
  
    (match board.placed with
    | [] -> ()
    | prev_loc :: _->
        let valid_moves = List.map (fun ((r, c), _) -> (r, c)) (get_loc_counts_from_loc board prev_loc) in
        if not (List.mem (row, col) valid_moves) then
          invalid_arg ("Location not knights move"));
  
    let new_placed = (row, col) :: board.placed in
    let new_indices = S.set board.indices (row, col) new_index in
    let new_reachable =  S.remove board.reachable (row, col) in

    let updated_reachable =
      List.fold_left (fun acc_storage ((r, c), count) ->
        S.set acc_storage (r, c) (count - 1)
      ) new_reachable (get_loc_counts_from_loc board (row, col))
    in
  
    { board with placed = new_placed; last_index = new_index;
                  indices = new_indices; reachable = updated_reachable }
    

    (* B.4 *)
    let undo board = 
      match board.placed with 
      | [] -> board
      | (row, col) :: t ->
        let new_prev_index = board.last_index - 1 in 
        let new_indices = S.remove board.indices (row, col) in
        let new_reachable = S.set board.reachable (row, col)  (List.length (get_loc_counts_from_loc board (row, col))) in

        let update_reachable = 
          List.fold_left (fun acc_storage ((r, c), count) -> 
            S.set acc_storage (r, c) (count +1)
            ) new_reachable(get_loc_counts_from_loc board (row, col))
          in 
          {board with placed = t; last_index = new_prev_index; indices = new_indices; reachable = update_reachable}


    let is_solved board = board.last_index = board.size
    let get_placed board = List.rev board.placed
  end
