open Asttypes
open Ast_helper
open Ast_mapper
open Parsetree

(* Extension checker *)

let if_extension (name, _) =
  name.txt = "if"

let else_extension (name, _) =
  name.txt = "else"


(* Helpers *)

let mk_ident id pos = Location.mkloc (Longident.parse id) pos

(** [mk_variable_length l one many] allows to create an object according to
    the number of elements in the given list [l].
    The list can't be empty. *)
let mk_variable_length l one many = match l with
  | []  -> assert false
  | [x] -> one x
  | xs  -> many xs

(** [mk_match_expression es] create an expression according to the number
    of expression in the list [es].
    If there is only one expression, it's return as is.
    If there is multiple expression, it's returned as a tuple. *)
let mk_match_expression es =
  mk_variable_length es (fun x -> x.pexp_desc) (fun xs -> Pexp_tuple xs)

(** [mk_matched_pattern ps] does the same thing as [mk_match_expression],
    but with a list of pattern. *)
let mk_matched_pattern ps =
  mk_variable_length ps (fun x -> x.ppat_desc) (fun xs -> Ppat_tuple xs)

(** [no_attr_expr pexp_desc pexp_loc] create an expression with the descendant
    [pexp_desc] at location [pexp_loc] without any attributes. *)
let no_attr_expr pexp_desc pexp_loc =
  { pexp_desc; pexp_loc; pexp_attributes = [] }

(** [no_attr_pat ppat_desc ppat_loc] does the same thing as [no_attr_expr] but
    with patterns. *)
let no_attr_pat ppat_desc ppat_loc =
  { ppat_desc; ppat_loc; ppat_attributes = [] }

(** [e_unit loc] create the unit expression. *)
let e_unit loc =
  no_attr_expr (Pexp_construct (mk_ident "()" loc, None)) loc

(** [p_any loc] create the pattern "_". *)
let p_any loc =
  no_attr_pat Ppat_any loc

(** [mk_case_no_guard pc_lhs pc_rhs] create a case of a match expression
    without guard expression (ie. when ...) *)
let mk_case_no_guard pc_lhs pc_rhs = { pc_lhs; pc_guard = None; pc_rhs }

let extract_expression payload = match payload with
  | PStr [{ pstr_desc = Pstr_eval (e, _) }] -> e
  | _ -> assert false

let bindings_as_pattern vbs loc =
  let pat_and_exp = List.map (fun { pvb_pat = p; pvb_expr = e } -> p, e) vbs in
  let lhs, rhs = List.split pat_and_exp in
  let matched_pat = no_attr_pat (mk_matched_pattern lhs) loc in
  let match_expr  = no_attr_expr (mk_match_expression rhs) loc in
  matched_pat, match_expr

let make_expression else_clause expr loc =
  match expr.pexp_desc with
  | Pexp_let (Nonrecursive, vbs, body) ->
    let (matched_pat, match_expr) = bindings_as_pattern vbs loc in
    let matched_case = mk_case_no_guard matched_pat body in
    let unmatched_expr = match else_clause with
      | None -> e_unit loc
      | Some pstr -> extract_expression pstr
    in
    let unmatched_case = mk_case_no_guard (p_any loc) unmatched_expr in
    { pexp_desc = Pexp_match (match_expr, [matched_case; unmatched_case]);
      pexp_loc = loc;
      pexp_attributes = [] }
  | _ ->
    assert false

let rewrite_payload ?(else_clause = None) payload = match payload with
  | PStr [{ pstr_desc = Pstr_eval (expression, _); pstr_loc }] ->
    make_expression else_clause expression pstr_loc
  | _ ->
    assert false (* put a correct error here. *)

let rewrite_expression default_mapper mapper expression =
  let { pexp_desc; pexp_loc; pexp_attributes } = expression in
  match pexp_desc with
  | Pexp_extension extension when if_extension extension ->
    rewrite_payload (snd extension)
  | Pexp_apply (
      { pexp_desc = Pexp_extension ext1 },
      [_, { pexp_desc = Pexp_extension ext2 }]
    ) when if_extension ext1 && else_extension ext2 ->
    rewrite_payload ~else_clause:(Some (snd ext2)) (snd ext1)
  | _ ->
    default_mapper.expr mapper expression
  
let letif argv = {
  default_mapper with
  expr = rewrite_expression default_mapper;
  (* structure_item = rewrite_structure_item default_mapper *)
}

let () = register "letif" letif
