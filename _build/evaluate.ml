(* Types for values, expressions, and types in a small expression 
   language.
 *)

type value 
  = IntV of int
  | BoolV of bool

and  expr 
  = Val of value

  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr

  | Lt  of expr * expr
  | Lte  of expr * expr
  | Eq  of expr * expr
  | And of expr * expr
  | Or  of expr * expr
  | Not of expr

  | Id of string
  | Let of string * typ * expr * expr

and  typ
  = IntT
  | BoolT

(* Evaluation *)

type environment = (string * value) list

exception DivisionByZero of value

let rec eval (e: expr) (env: environment) : value =
  match e with
  | Val v -> v 
  | Add (e1, e2) -> 
     (match eval e1 env, eval e2 env with
      | IntV i1, IntV i2 -> IntV (i1 + i2)
      | _ -> raise (Failure "Internal Error on Add")
     )
  | Sub (e1, e2) -> 
  (match eval e1 env, eval e2 env with
    | IntV i1, IntV i2 -> IntV (i1 - i2)
    | _ -> raise (Failure "Internal Error on Add")
  )
  | Mul (e1, e2) -> 
    (match eval e1 env, eval e2 env with
      | IntV i1, IntV i2 -> IntV (i1 * i2)
      | _ -> raise (Failure "Internal Error on Add")
    )
  | Div (e1, e2) -> 
    (match eval e1 env, eval e2 env with
      | IntV i1, IntV i2 -> if i2 == 0 then raise(DivisionByZero (IntV i1)) else IntV (i1 / i2)
      | _ -> raise (Failure "Internal Error on Add")
    )
  | Lt (e1, e2) -> 
     (match eval e1 env, eval e2 env with
      | IntV i1, IntV i2 -> BoolV (i1 < i2)
      | _ -> raise (Failure "Internal Error on Lt")
     )
  | Lte (e1, e2) -> 
  (match eval e1 env, eval e2 env with
    | IntV i1, IntV i2 -> BoolV (i1 <= i2)
    | _ -> raise (Failure "Internal Error on Lte")
  )
  | Eq (e1, e2) -> 
    (match eval e1 env, eval e2 env with
     | IntV i1, IntV i2 -> BoolV (i1 = i2)
     | _ -> raise (Failure "Internal Error on Eq")
  )
   | And (e1, e2) -> 
    (match eval e1 env, eval e2 env with
      | BoolV b1, BoolV b2 -> BoolV (b1 && b2)
      | _ -> raise (Failure "Internal Error on And")
    )
  | Or (e1, e2) -> 
    (match eval e1 env, eval e2 env with
      | BoolV b1, BoolV b2 -> BoolV (b1 || b2)
      | _ -> raise (Failure "Internal Error on Or")
    )
  | Not e1 -> 
    (match eval e1 env with
      | BoolV b1 -> if b1 = true then BoolV(b1 = false) else BoolV(b1 = true)
      | _ -> raise (Failure "Internal Error on Not")
    )
  | Id s -> let rec lookup (str:string) (envir:environment) : value = 
    match envir with
    | [] -> raise (Failure "Internal Error on Id")
    | (n, v)::t when s=n -> v
    | (n, v)::t -> lookup s t
    in lookup s env

  | Let (s1, t1, e1, e2) -> 
     match eval e1 env with
    | IntV i1 ->  eval e2 ((s1, IntV i1)::env)
    | BoolV b1 -> eval e2 ((s1, BoolV b1)::env)


type context = (string * typ) list

type typ_error = ExpectedInt | ExpectedBool | UndeclaredName

type 'a result = Result of 'a
               | Error of typ_error

let rec type_check (e: expr) (ctx: context) : typ result =
  let rec lookup (str:string) (conte:context) : typ result = 
    match conte with
    | [] -> Error UndeclaredName
    | (n, v)::t when str=n -> Result v
    | (n, v)::t -> lookup str t
    in 
  match e with
  | Val (IntV _) -> Result IntT
  | Val (BoolV _) -> Result BoolT
  | Add (e1, e2) | Sub (e1, e2) | Mul (e1, e2) | Div (e1, e2) -> 
     (match type_check e1 ctx, type_check e2 ctx with
      | Result IntT, Result IntT -> Result IntT
      | Result BoolT, _ -> Error ExpectedInt
      | Result IntT, Result BoolT -> Error ExpectedInt
      | Error err, _ -> Error err
      | _, Error err -> Error err
     )
  | Lt(e1, e2) | Lte(e1,e2) | Eq(e1, e2) -> 
      (match type_check e1 ctx, type_check e2 ctx with
      | Result IntT, Result IntT -> Result BoolT (*questionable. maybe output Result IntT*)
      | Error err, _ -> Error err
      | _, Error err -> Error err
      | _-> Error ExpectedInt)
  | And(e1, e2) | Or(e1, e2) -> 
    (match type_check e1 ctx, type_check e2 ctx with
    | Result BoolT, Result BoolT -> Result BoolT
    | Error err, _ -> Error err
    | _, Error err -> Error err
    | _-> Error ExpectedBool)
  | Not e1 -> 
    (match type_check e1 ctx with
    | Result BoolT -> Result BoolT
    | Error err -> Error err
    | _-> Error ExpectedBool)
  | Id s -> lookup s ctx 


  | Let (str, typ1, e1, e2) -> (match type_check e1 ctx with 
    | Result IntT -> if typ1 = IntT then type_check e2 ((str, IntT)::ctx) else Error ExpectedBool
    | Result BoolT -> if typ1 = BoolT then type_check e2 ((str, BoolT)::ctx) else Error ExpectedInt
    | Error err -> Error err )





(* This function should never raise an exception, even though a function
   that it calls will raise exceptions on some of its inputs.
   Why is that?
 *)
let evaluate (e: expr) : value result =
  match type_check e [] with
  | Result ty -> Result (eval e [])
  | Error err -> Error err

