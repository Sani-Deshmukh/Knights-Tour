open Storage
open Board
open Utils

exception Solution_not_found

module type Searcher =
  sig
    val search : int -> int -> int -> int -> bool -> (Loc.t list) option
  end

module Make (S : Storage) : Searcher =
  struct
    module B = Board.Make(S)
    module P = Print(B)

    (* Helper functions go here. *)
    let random_picker lst = 
      let len = List.length lst in
      List.nth lst (Random.int len)
    
    let rec helper b = 
      if B.is_solved b then Some (B.get_placed b) 
        else
          let last_loc = B.get_last b in 
          let moves = B.get_loc_counts_from_loc b last_loc in 
          if moves = [] then raise Solution_not_found
          else 
            let min_reach = List.fold_left(fun acc (_, count) -> min acc count) max_int moves in 
            let min_reach_moves = List.filter( fun (_, count) -> count = min_reach) moves in 
            let best_move = random_picker min_reach_moves in 
            let next_loc = fst(best_move) in 
            let b' = B.place b next_loc in 
            helper b'
    
    (* Part C *)
    let search nrows ncols start_row start_col print =
        let board = B.make nrows ncols in 
        let board = B.place board (start_row, start_col) in 
        
        try 
          let res = helper board in 
          match res with 
          | Some sol -> 
            if print then P.print_board board true;
            Some sol
          | None -> None
        with
        | Solution_not_found -> None
  end

