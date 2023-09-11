
open Tests
open Evaluate

let show_value (v: value) : string = match v with
  | IntV i -> "IntV " ^ Int.to_string i
  | BoolV b -> "BoolV " ^ Bool.to_string b

let show_typ (t: typ) : string = match t with
  | IntT -> "IntT"
  | BoolT -> "BoolT"

let show_err (err: typ_error) : string = match err with
  | ExpectedInt -> "ExpectedInt"
  | ExpectedBool -> "ExpectedBool"
  | UndeclaredName -> "UndeclaredName"

let show_result (r: typ result) : string = match r with
  | Result t -> "Result " ^ show_typ t
  | Error err -> "Error " ^ show_err err

let show_excn e = match e with
  | Failure s -> "Failure \"" ^ s ^ "\""
  | DivisionByZero v -> "DivisionByZero (" ^ show_value v ^ ")"
  | Match_failure _ -> "Match_failure exception, missing some patterns"
  | _ -> raise (Failure "unknown")

let some_tests =
  [
    eval_test (fun () ->
        eval (Add (Val (IntV 3), Val (IntV 4))) [], IntV 7)
       "eval (Add (Val (IntV 3), Val (IntV 4))) []"
        show_value;

    excp_test (fun () ->
        eval (Add (Val (IntV 3), Val (BoolV false))) [])
       "eval (Add (Val (IntV 3), Val (BoolV false))) []"
        show_value
        show_excn
        (Failure "Internal Error on Add");

    excp_test (fun () ->
        eval (Div (Add (Val (IntV 3), Val (IntV 7)), Sub (Val (IntV 2), Val (IntV 2)))) [])
       "eval (Div (Add (Val (IntV 3), Val (IntV 7)), Sub (Val (IntV 2), Val (IntV 2)))) []"
        show_value
        show_excn
        (DivisionByZero (IntV 10));

    eval_test (fun () ->
        eval (Sub (Val (IntV 13), Val (IntV 6))) [], IntV 7)
       "eval (Sub (Val (IntV 13), Val (IntV 6))) []"
        show_value;

    eval_test (fun () ->
        eval (Lte (Val (IntV 13), Val (IntV 6))) [], BoolV false)
       "eval (Lte (Val (IntV 13), Val (IntV 6))) []"
        show_value;

    eval_test (fun () ->
        eval (Lte (Val (IntV 24), Val (IntV 24))) [], BoolV true)
       "eval (Lte (Val (IntV 24), Val (IntV 24))) []"
        show_value;

    eval_test (fun () ->
        eval (Let ("x", IntT, Add (Val (IntV 3), Val (IntV 4)), Sub (Val (IntV 10), Id "x"))) [], IntV 3)
       "eval (Let (\"x\", IntT, Add (Val (IntV 3), Val (IntV 4)), Sub (Val (IntV 10), Id \"x\"))) []"
        show_value;

     excp_test (fun () ->
        eval (Let ("x", IntT, Val (IntV 10), Add (Val (IntV 5), Id "y"))) [])
       "Let (\"x\", IntT, Val (IntV 10), Add (Val (IntV 5), Id \"y\"))"
        show_value
        show_excn
        (Failure "Internal Error on Id");

    eval_test (fun () ->
        eval (Let ("x", BoolT, Add (Val (IntV 3), Val (IntV 4)), Sub (Val (IntV 10), Id "x"))) [], IntV 3)
       "eval (Let (\"x\", BoolT, Add (Val (IntV 3), Val (IntV 4)), Sub (Val (IntV 10), Id \"x\"))) []"
        show_value;


    eval_test (fun () ->
        type_check (Add (Val (IntV 3), Val (IntV 4))) [], Result IntT)
       "type_check (Add (Val (IntV 3), Val (IntV 4))) []"
        show_result;

    eval_test (fun () ->
        type_check (Add (Val (IntV 3), Val (BoolV false))) [], 
        Error ExpectedInt)
       "type_check (Add (Val (IntV 3), Val (BoolV false))) []"
        show_result;

    eval_test (fun () ->
        type_check (Let ("x", IntT, Add (Val (IntV 3), Val (IntV 4)), Sub (Val (IntV 10), Id "x"))) [], Result IntT)
       "type_check (Let (\"x\", IntT, Add (Val (IntV 3), Val (IntV 4)), Sub (Val (IntV 10), Id \"x\"))) []"
        show_result;


    eval_test (fun () ->
        type_check (Let ("x", IntT, Val (IntV 10), Add (Val (IntV 5), Id "y"))) [], Error UndeclaredName)
       "type_check (Let (\"x\", IntT, Val (IntV 10), Add (Val (IntV 5), Id \"y\"))) []"
        show_result;


    eval_test (fun () ->
        type_check (Let ("x", BoolT, Add (Val (IntV 3), Val (IntV 4)), Sub (Val (IntV 10), Id "x"))) [], Error ExpectedBool)
       "type_check (Let (\"x\", BoolT, Add (Val (IntV 3), Val (IntV 4)), Sub (Val (IntV 10), Id \"x\"))) []"
        show_result;

    eval_test (fun () ->
        type_check (Let ("b", IntT, And (Val (BoolV true), Val (BoolV false)), Or (Val (BoolV true), Id "b"))) [], Error ExpectedInt)
       "        type_check (Let (\"b\", IntT, And (Val (BoolV true), Val (BoolV false)), Or (Val (BoolV true), Id \"b\"))) []"
        show_result; 

  ]

let run_tests tests = List.iter (fun f -> f ()) tests

let _ = run_tests some_tests

