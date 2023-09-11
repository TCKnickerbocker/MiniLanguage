open Tests
open Execute

(* Some sample programs *)
let program_assign =
  (* x := 1;
     y := x + 2;
   *)
  ("assign", 
   ["x"; "y"],
   Seq (Assign ("x", Val (Int 1)),
        Assign ("y", Add (Id  "x", Val (Int 2)))
     )
  )


let program_seq =
  (*   x := 1 ;
       x := x + 1 ;
       x := x + 1 ;
       x := x + 1 ;
       x := x + 1 ;
       x := x + 1 ;
   *)
  ("program_seq", ["x"],
  Seq (Assign ("x", Val (Int 1)),
  Seq (Assign ("x", Add (Id "x", Val (Int 1))),
  Seq (Assign ("x", Add (Id "x", Val (Int 1))),
  Seq (Assign ("x", Add (Id "x", Val (Int 1))),
  Seq (Assign ("x", Add (Id "x", Val (Int 1))),
       Assign ("x", Add (Id "x", Val (Int 1))))))))
  )


let program_ifthenelse_simple =
  (* y = 10
     if y < 15 then result := 10 else result := 11
   *)
  ("ifthen_else_simple",
   ["y"; "result"],
   Seq (Assign ("y", Val (Int 10)),
        IfThenElse 
          (Lt (Id  "y", Val (Int 15)),
           Assign ("result", Val (Int 10)),
           Assign ("result", Val (Int 11))
          )
     )
  )



(* A few versions of summing up to 10 using different forms of loops. *)
let program_sum_to_10_while =
  (* i = 0;
     sum = 0;
     while (i <= 10) {
       sum = sum + i;
       i = i + 1
     }
   *)
  ("sum_10_while",
   ["i"; "sum"],
   Seq (Assign ("i", Val (Int 0)),
   Seq (Assign ("sum", Val (Int 0)),
   Seq (While (Lte (Id  "i", Val (Int 10)),
               Seq (Assign ("sum", Add (Id  "sum", Id  "i")),
                    Assign ("i", Add (Id  "i", Val (Int 1))) ) 
          ),
   Seq (WriteStr "sum is",
        WriteNum (Id  "sum")
       )
     )
     )
     )
  )

let program_sum_to_10_repeat =
  (* i = 0;
     sum = 0
     repeat
       sum = sum + i
       i = i + 1
     until i > 10;
     write "sum is:";
     write sum
   *)
  ("sum_10_repeat",
   ["i"; "sum"],
   Seq (Assign ("i", Val (Int 0)),
   Seq (Assign ("sum", Val (Int 0)),
   Seq (RepeatUntil (Seq (Assign("sum", Add (Id  "sum", Id  "i")),
                          Assign ("i", Add (Id  "i", Val (Int 1)))
                       ),
                     Gt (Id "i", Val (Int 10))
          ),
   Seq (WriteStr "sum is",
        WriteNum (Id  "sum")
       )
     )
     )
     )
  )

let program_sum_to_10_do_while =
  (* i = 0;
     sum = 0;
     do
       sum = sum + i;
       i = i + 1
     while (i <= 10) 
   *)
  ("sum",
   ["i"; "sum_10_do_while"],
   Seq (Assign ("i", Val (Int 0)),
   Seq (Assign ("sum", Val (Int 0)),
   Seq (DoWhile (Seq (Assign ("sum", Add (Id  "sum", Id  "i")),
                      Assign ("i", Add (Id  "i", Val (Int 1))) 
                     ),
                 Lte (Id  "i", Val (Int 10))
                ),
   Seq (WriteStr "sum is",
        WriteNum (Id  "sum")
     )
     )
     )
     )
  )

let program_sum_to_10_for =
  (* sum = 0
     for i = 0 to 10
       sum = sum + i
     write "sum is:";
     write sum
   *)
  ("sum_10_for",
   ["sum"; "i"],
   Seq (Assign ("sum", Val (Int 0)),
   Seq (For ("i", Val (Int 0), Val (Int 10),
             Assign("sum", Add (Id  "sum", Id  "i"))),
   Seq (WriteStr "sum is",
        WriteNum (Id  "sum")
       ) ) )
  )

let some_tests =
  [
    eval_test (fun () ->
        lookup "x" [ ("x", 1) ], 1)
       "lookup \"x\" [ (\"x\", 1) ], 1)"
        Int.to_string;

    eval_test (fun () ->
        lookup "x" [ ("x", 10); ("x", 9); ("x", 8) ], 10)
       "lookup \"x\" [ (\"x\", 10); (\"x\", 9); (\"x\", 8) ], 10)"
        Int.to_string;

    eval_test (fun () ->
        lookup "x" (run program_assign), 1)
       "lookup \"x\" (run program_assign), 1)"
        Int.to_string;

    eval_test (fun () ->
        lookup "y" (run program_assign), 3)
       "lookup \"y\" (run program_assign), 3)"
        Int.to_string;

    eval_test (fun () ->
        lookup "x" (run program_seq), 6)
       "lookup \"x\" (run program_seq), 6)"
        Int.to_string;

    eval_test (fun () ->
        lookup "result" (run program_ifthenelse_simple), 10)
       "lookup \"result\" (run program_ifthenelse_simple), 10)"
        Int.to_string;

    eval_test (fun () ->
        lookup "sum" (run program_sum_to_10_while), 55)
       "lookup \"sum\" (run program_sum_to_10_while), 55)"
        Int.to_string;

    eval_test (fun () ->
        lookup "sum" (run program_sum_to_10_repeat), 55)
       "lookup \"sum\" (run program_sum_to_10_repeat), 55)"
        Int.to_string;

    eval_test (fun () ->
        lookup "sum" (run program_sum_to_10_do_while), 55)
       "lookup \"sum\" (run program_sum_to_10_do_while), 55)"
        Int.to_string;

    eval_test (fun () ->
        lookup "sum" (run program_sum_to_10_for), 55)
       "lookup \"sum\" (run program_sum_to_10_for), 55)"
        Int.to_string;
  ]

let run_tests tests = List.iter (fun f -> f ()) tests

let _ = run_tests some_tests
