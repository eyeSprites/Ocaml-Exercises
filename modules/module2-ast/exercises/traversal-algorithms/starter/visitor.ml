(* visitor.ml - AST visitor pattern exercises.
   Implement two common visitor-style operations that walk the AST
   and accumulate information. *)

open Shared_ast.Ast_types

(** Count the number of each node type in a statement list.
    Returns an association list like:
      [("Assign", 3); ("IntLit", 5); ("BinOp", 2); ...]
    The keys are constructor names WITHOUT parameters (e.g., "IntLit"
    not "IntLit(3)"). Order does not matter.

    Hint:
      - Write recursive helpers for expr and stmt.
      - Use a mutable Hashtbl or a ref to a Map to accumulate counts,
        or thread an accumulator through the recursion.
      - Don't forget to count the node itself AND recurse into its
        children. *)
let count_nodes (stmts : stmt list) : (string * int) list =
  let counts = Hashtbl.create 16 in
  let inc key =
    let old = Option.value (Hashtbl.find_opt counts key) ~default:0 in
    Hashtbl.replace counts key (old + 1)
  in
  let rec walk_expr (e : expr) : unit =
    match e with
    | IntLit _ -> inc "IntLit"
    | BoolLit _ -> inc "BoolLit"
    | Var _ -> inc "Var"
    | BinOp (_, e1, e2) ->
      inc "BinOp";
      walk_expr e1;
      walk_expr e2
    | UnaryOp (_, e1) ->
      inc "UnaryOp";
      walk_expr e1
    | Call (_, args) ->
      inc "Call";
      List.iter walk_expr args
  in
  let rec walk_stmt (s : stmt) : unit =
    match s with
    | Assign (_, e) ->
      inc "Assign";
      walk_expr e
    | If (cond, then_b, else_b) ->
      inc "If";
      walk_expr cond;
      List.iter walk_stmt then_b;
      List.iter walk_stmt else_b
    | While (cond, body) ->
      inc "While";
      walk_expr cond;
      List.iter walk_stmt body
    | Return None ->
      inc "Return"
    | Return (Some e) ->
      inc "Return";
      walk_expr e
    | Print exprs ->
      inc "Print";
      List.iter walk_expr exprs
    | Block body ->
      inc "Block";
      List.iter walk_stmt body
  in
  List.iter walk_stmt stmts;
  Hashtbl.fold (fun k v acc -> (k, v) :: acc) counts []

(** Evaluate a constant expression, returning Some int if the
    expression contains only integer literals and arithmetic operators,
    or None if it contains variables, booleans, calls, or comparison
    operators.

    Supported operators: Add, Sub, Mul, Div (integer division).
    Division by zero should return None.

    Examples:
      evaluate (IntLit 42)                        => Some 42
      evaluate (BinOp (Add, IntLit 1, IntLit 2))  => Some 3
      evaluate (BinOp (Add, IntLit 1, Var "x"))   => None
      evaluate (BoolLit true)                      => None

    Hint: use Option.bind or match on recursive results. *)
let rec evaluate (e : expr) : int option =
  match e with
  | IntLit n -> Some n
  | BoolLit _ -> None
  | Var _ -> None
  | Call _ -> None
  | UnaryOp _ -> None
  | BinOp (op, e1, e2) ->
    begin match op, evaluate e1, evaluate e2 with
    | Add, Some n1, Some n2 -> Some (n1 + n2)
    | Sub, Some n1, Some n2 -> Some (n1 - n2)
    | Mul, Some n1, Some n2 -> Some (n1 * n2)
    | Div, Some _, Some 0 -> None
    | Div, Some n1, Some n2 -> Some (n1 / n2)
    | _ -> None
    end
