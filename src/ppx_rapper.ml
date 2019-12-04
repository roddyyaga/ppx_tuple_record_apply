open Base
open Ppxlib

let lident_of_field field =
  Ast_builder.Default.Located.lident ~loc:field.pld_name.loc field.pld_name.txt

(** Defines the returned expression in [let t_to_tuple {a; b; c} = (a, b, c)] *)
let tuple_expression (module Ast : Ast_builder.S) fields =
  let fields =
    List.map fields ~f:(fun field -> Ast.pexp_ident (lident_of_field field))
  in
  Ast.pexp_tuple fields

(** Defines the parameter pattern in [let t_to_tuple {a; b; c} = (a, b, c)] *)
let record_pattern (module Ast : Ast_builder.S) fields =
  let fields =
    List.map fields ~f:(fun field ->
        let pattern = Ast.pvar field.pld_name.txt in
        let field_id = lident_of_field field in
        (field_id, pattern))
  in
  Ast.ppat_record fields Closed

(** Defines the parameter pattern in [let t_of_tuple (a, b, c) = {a; b; c}] *)
let tuple_pattern (module Ast : Ast_builder.S) fields =
  let fields = List.map fields ~f:(fun field -> Ast.pvar field.pld_name.txt) in
  Ast.ppat_tuple fields

(** Defines the returned expression in [let t_of_tuple (a, b, c) = {a; b; c}] *)
let record_expression (module Ast : Ast_builder.S) fields =
  let fields =
    List.map fields ~f:(fun field ->
        let field_id = lident_of_field field in
        let expression = Ast.pexp_ident field_id in
        (field_id, expression))
  in

  Ast.pexp_record fields None

(** Given the fields in [{a: int; b: string; c: bool}], generates [~a ~b ~c] *)
let labelled_args (module Ast : Ast_builder.S) fields =
  let _ = Ast.pexp_apply in
  let f field =
    let name = field.pld_name.txt in
    let field_id = lident_of_field field in
    let expression = Ast.pexp_ident field_id in
    (Labelled name, expression)
  in
  List.map fields ~f

(** Generates a name for a function from a type and some suffix. *)
let function_name (module Ast : Ast_builder.S) t suffix =
  let type_name = t.ptype_name.txt in
  (match type_name with "t" -> suffix | other -> other ^ "^" ^ suffix)
  |> Ast.pvar

(** Generates [type tuple = int * string * bool] for some record type [{a: int; b: string; c: bool}]*)
let tuple_type_definition (module Ast : Ast_builder.S) fields ~loc =
  let type_list = List.map fields ~f:(fun field -> field.pld_type) in
  let types = Ast.ptyp_tuple type_list in
  [%stri type tuple = [%t types]]

let labelled_function_type_definition (module Ast : Ast_builder.S) fields ~loc =
  let f field next_type =
    let name = field.pld_name.txt in
    Ast.ptyp_arrow (Labelled name) field.pld_type next_type
  in
  let def = List.fold_right fields ~f ~init:(Ast.ptyp_var "a") in
  [%stri type 'a labelled_function = [%t def]]

let to_tuple_str_gen (module Ast : Ast_builder.S) t fields ~loc =
  let record_pat = record_pattern (module Ast) fields in
  let tuple_expr = tuple_expression (module Ast) fields in
  let name = function_name (module Ast) t "to_tuple" in
  [%stri let [%p name] = fun [%p record_pat] -> [%e tuple_expr]]

let of_tuple_str_gen (module Ast : Ast_builder.S) t fields ~loc =
  let tuple_pat = tuple_pattern (module Ast) fields in
  let record_expr = record_expression (module Ast) fields in
  let name = function_name (module Ast) t "of_tuple" in
  [%stri let [%p name] = fun [%p tuple_pat] -> [%e record_expr]]

let apply_str_gen (module Ast : Ast_builder.S) t fields ~loc =
  let record_pat = record_pattern (module Ast) fields in
  let name = function_name (module Ast) t "record_apply" in
  let application =
    let applied_function = Ast.pexp_ident { txt = Lident "f"; loc } in
    let arguments = labelled_args (module Ast) fields in
    Ast.pexp_apply applied_function arguments
  in
  [%stri let [%p name] = fun f [%p record_pat] -> [%e application]]

let str_gen ~loc ~path:_ (_rec, t) =
  let (module Ast) = Ast_builder.make loc in
  let t = List.hd_exn t in
  let fields =
    match t.ptype_kind with
    | Ptype_record fields -> fields
    | _ ->
        Location.raise_errorf ~loc "ppx_rapper should be used on record types"
  in
  let tuple_type_str = tuple_type_definition (module Ast) fields ~loc in
  let labelled_function_type_str =
    labelled_function_type_definition (module Ast) fields ~loc
  in
  let to_tuple_str = to_tuple_str_gen (module Ast) t fields ~loc in
  let of_tuple_str = of_tuple_str_gen (module Ast) t fields ~loc in
  let apply_str = apply_str_gen (module Ast) t fields ~loc in
  [
    tuple_type_str;
    to_tuple_str;
    of_tuple_str;
    labelled_function_type_str;
    apply_str;
  ]

let str_type_decl = Deriving.Generator.make_noarg str_gen

let name = "rapper"

let () = Deriving.add name ~str_type_decl |> Deriving.ignore
