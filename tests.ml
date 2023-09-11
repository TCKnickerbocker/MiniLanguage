type test = unit -> unit


let show_list (show_elem: 'a -> string) (lst: 'a list) : string =
  (* A function to convert lists to strings. *)
  "[" ^ (String.concat "; " (List.map show_elem lst)) ^ "]"

let show_option (show_elem: 'a -> string) (o: 'a option) : string =
  (* A function to convert options to strings. *)
  match o with
  | None -> "None"
  | Some x -> "Some (" ^ show_elem x ^ ")"

let id x = x

let eval_test (f: unit -> ('a * 'a)) (expr_str: string)
      (show: 'a -> string) : test =
  fun () ->
  (try
     let expr, expected = f ()
     in
     if expr = expected then
       let msg = "PASSED:\n    `" ^ expr_str ^ 
                   "`\n correctly evaluates to\n    `" ^
                     show expected ^ "`"
       in
       print_endline msg
     else
       let msg = "FAILED:\n    `" ^ expr_str ^ 
                   "`\n  incorrectly evaluates to\n    `" ^
                     show expr ^ "`\n  but should have evaluated to\n    `" ^
                     show expected ^ "`"
       in
       print_endline msg
   with
   | _ ->
      let msg = "FAILED:\n    `" ^ expr_str ^ 
                   "`\n  raised an exception but should have computed a value."
       in
       print_endline msg
  )

let excp_test (f: unit -> 'a) (expr_str: string) 
      (show: 'a -> string) (show_excn: exn -> string) (expected: exn) : test =
  let show_e e = try "`" ^ show_excn e ^ "`" with
    | _ -> " an unexpected exception "
  in
  (fun () ->
  try
    let v = f () in
    let msg = "FAILED:\n    `" ^ expr_str ^ 
                "`\n  should have raised an exception " ^
                "but instead evaluated to\n    `" ^
                show v ^ "`"
    in
    print_endline msg
  with 
  | e ->
     if e = expected 
     then
       let msg = "PASSED:\n    `" ^ expr_str ^
                   "`\n  correctly raised the exception\n    `" ^
                   show_excn e ^ "`"
       in print_endline msg
     else
       let msg = "FAILED:\n    `" ^ expr_str ^
                   "`\n  raised\n   " ^ show_e e ^ 
                   "\n  but should have raised the exception\n   `" ^ 
                   show_excn expected ^ "`"
       in print_endline msg
   )    


let run_tests tests = List.iter (fun f -> f ()) tests
