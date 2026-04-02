(* traversals.ml - AST traversal algorithms exercise.
   Implement three classic tree traversal strategies on the AST:
   pre-order (depth-first), post-order (depth-first), and
   breadth-first (level-order).

   Each function walks a list of statements and collects a string label
   for every node visited. Labels should look like:
     Statements: "Assign", "If", "While", "Return", "Print", "Block"
     Expressions: "IntLit(3)", "BoolLit(true)", "Var(x)", "BinOp(+)",
                  "UnaryOp(-)", "Call(f)"
*)

open Shared_ast.Ast_types

(** Helper: produce a string label for a single expression node.
    Examples: IntLit(3), BoolLit(true), Var(x), BinOp(+), UnaryOp(-), Call(f) *)
let label_of_expr (_e : expr) : string =
  (* TODO: pattern match on the expression and return its label string *)
  match _e with
  | IntLit n -> Printf.sprintf "IntLit(%d)" n
  | BoolLit b -> Printf.sprintf "BoolLit(%b)" b
  | Var v -> Printf.sprintf "Var(%s)" v
  | BinOp (op, _, _) ->
    let s = match op with
      | Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/"
      | Eq -> "==" | Neq -> "!=" | Lt -> "<" | Gt -> ">" | Le -> "<=" | Ge -> ">="
      | And -> "&&" | Or -> "||"
    in
    Printf.sprintf "BinOp(%s)" s
  | UnaryOp (uop, _) ->
    let s = match uop with Neg -> "-" | Not -> "!" in
    Printf.sprintf "UnaryOp(%s)" s
  | Call (f, _) -> Printf.sprintf "Call(%s)" f

(** Helper: produce a string label for a single statement node.
    Examples: "Assign", "If", "While", "Return", "Print", "Block" *)
let label_of_stmt (_s : stmt) : string =
  (* TODO: pattern match on the statement and return its label string *)
  match _s with
  | Assign _ -> "Assign"
  | If _ -> "If"
  | While _ -> "While"
  | Return _ -> "Return"
  | Print _ -> "Print"
  | Block _ -> "Block"

(** Pre-order depth-first traversal.
    Visit the current node FIRST, then recurse into its children
    left-to-right.

    For example, on [Assign("x", BinOp(Add, IntLit 1, IntLit 2))]:
      ["Assign"; "BinOp(+)"; "IntLit(1)"; "IntLit(2)"]

    Hint: write a mutual recursion with helpers for expr and stmt lists. *)
let pre_order (_stmts : stmt list) : string list =
  (* TODO: implement pre-order DFS traversal *)
  (* 1. Emit the label of the current node
     2. Then recurse into children *)
  let rec visit_expr e =
    let here = [label_of_expr e] in
    match e with
    | IntLit _ | BoolLit _ | Var _ -> here
    | BinOp (_, e1, e2) -> here @ visit_expr e1 @ visit_expr e2
    | UnaryOp (_, e1) -> here @ visit_expr e1
    | Call (_, args) -> here @ List.concat_map visit_expr args
  and visit_stmt s =
    let here = [label_of_stmt s] in
    match s with
    | Assign (_, e) -> here @ visit_expr e
    | If (c, t, e) -> here @ visit_expr c @ visit_stmts t @ visit_stmts e
    | While (c, b) -> here @ visit_expr c @ visit_stmts b
    | Return None -> here
    | Return (Some e) -> here @ visit_expr e
    | Print exprs -> here @ List.concat_map visit_expr exprs
    | Block ss -> here @ visit_stmts ss
  and visit_stmts ss =
    List.concat_map visit_stmt ss
  in
  visit_stmts _stmts

(** Post-order depth-first traversal.
    Recurse into children FIRST, then visit the current node.

    For example, on [Assign("x", BinOp(Add, IntLit 1, IntLit 2))]:
      ["IntLit(1)"; "IntLit(2)"; "BinOp(+)"; "Assign"]

    Hint: same structure as pre_order but emit the label at the end. *)
let post_order (_stmts : stmt list) : string list =
  (* TODO: implement post-order DFS traversal *)
  (* 1. Recurse into children first
     2. Then emit the label of the current node *)
  let rec visit_expr e =
    let here = [label_of_expr e] in
    match e with
    | IntLit _ | BoolLit _ | Var _ -> here
    | BinOp (_, e1, e2) -> visit_expr e1 @ visit_expr e2 @ here
    | UnaryOp (_, e1) -> visit_expr e1 @ here
    | Call (_, args) -> List.concat_map visit_expr args @ here
  and visit_stmt s =
    let here = [label_of_stmt s] in
    match s with
    | Assign (_, e) -> visit_expr e @ here
    | If (c, t, e) -> visit_expr c @ visit_stmts t @ visit_stmts e @ here
    | While (c, b) -> visit_expr c @ visit_stmts b @ here
    | Return None -> here
    | Return (Some e) -> visit_expr e @ here
    | Print exprs -> List.concat_map visit_expr exprs @ here
    | Block ss -> visit_stmts ss @ here
  and visit_stmts ss =
    List.concat_map visit_stmt ss
  in
  visit_stmts _stmts

type node =
  | S of stmt
  | E of expr

(** Breadth-first (level-order) traversal.
    Visit all nodes at depth d before any node at depth d+1.

    For example, on [Assign("x", BinOp(Add, IntLit 1, IntLit 2))]:
      ["Assign"; "BinOp(+)"; "IntLit(1)"; "IntLit(2)"]
    (In this small case it happens to match pre-order, but differs on
     deeper trees with multiple siblings.)

    Hint: use the OCaml Queue module.
      1. Seed the queue with all top-level stmts.
      2. Dequeue a node, emit its label, enqueue its children.
      3. Repeat until the queue is empty.
    You will need a sum type or two queues to handle both stmt and expr
    nodes uniformly. *)
let bfs (_stmts : stmt list) : string list =
  (* TODO: implement breadth-first traversal using Queue *)
  let q = Queue.create () in
  List.iter (fun s -> Queue.add (S s) q) _stmts;
  let rec loop acc_rev =
    if Queue.is_empty q then List.rev acc_rev
    else
      match Queue.take q with
      | S s ->
        (match s with
         | Assign (_, e) -> Queue.add (E e) q
         | If (c, t, e) ->
           Queue.add (E c) q;
           List.iter (fun st -> Queue.add (S st) q) t;
           List.iter (fun st -> Queue.add (S st) q) e
         | While (c, b) ->
           Queue.add (E c) q;
           List.iter (fun st -> Queue.add (S st) q) b
         | Return None -> ()
         | Return (Some e) -> Queue.add (E e) q
         | Print exprs -> List.iter (fun e -> Queue.add (E e) q) exprs
         | Block ss -> List.iter (fun st -> Queue.add (S st) q) ss);
        loop (label_of_stmt s :: acc_rev)
      | E e ->
        (match e with
         | IntLit _ | BoolLit _ | Var _ -> ()
         | BinOp (_, e1, e2) -> Queue.add (E e1) q; Queue.add (E e2) q
         | UnaryOp (_, e1) -> Queue.add (E e1) q
         | Call (_, args) -> List.iter (fun a -> Queue.add (E a) q) args);
        loop (label_of_expr e :: acc_rev)
  in
  loop []
