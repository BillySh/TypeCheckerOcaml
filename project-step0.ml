(**************************************************************)
(**************************  Syntax ***************************)
(**************************************************************)

type program = exp
and exp = 
  | TRUE
  | FALSE
  | CONST of int
  | VAR of var
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | ISZERO of exp
  | IF of exp * exp * exp
  | LET of var * exp * exp
  | LETREC of var * var * exp * exp
  | PROC of var * exp
  | CALL of exp * exp
and var = string

type value = 
  | Int of int 
  | Bool of bool
  | Procedure of var * exp * env 
  | RecProcedure of var * var * exp * env 
and env = (var * value) list

(**************************************************************)
(************************ Environment *************************)
(**************************************************************)
let empty_env = []
let extend_env (x,v) e = (x,v)::e
let rec apply_env x e = 
  match e with
  | [] -> raise (Failure ("variable " ^ x ^ " not found"))
  | (y,v)::tl -> if x = y then v else apply_env x tl
(**************************************************************)
(***********************  Print values ************************)
(**************************************************************)
let rec to_string v = 
  match v with
  | Int n -> string_of_int n
  | Bool b -> string_of_bool b
  | _ -> "(function)"
let print_value v = print_endline (to_string v)


(**************************************************************)
(* Step 0. Develop an interpreter for the basic Proc-Language *)
(**************************************************************)

let rec eval (exp:exp) (env:env): value = 
  match exp with
  | CONST n -> Int n
  | TRUE -> Bool true
  | FALSE -> Bool false
  | VAR x -> apply_env x env
  | ADD (e1,e2) -> binop ( + ) e1 e2 env (*My modifications*)
  | SUB (e1,e2) -> binop ( - ) e1 e2 env (*binop was jus to keep the convention*)
  | MUL (e1,e2) -> binop ( * ) e1 e2 env
  | DIV (e1,e2) -> binop ( / ) e1 e2 env
  | IF (e1,e2,e3) -> (match (eval e1 env) with (*What we take is three exp, which are the original comparison, then, else*)
    | Bool true -> (eval e2 env) (*Here then applies*)
    | Bool false -> (eval e3 env) (*Here else applies*)
    | _ -> raise(Failure "Type Error ")) 
  | ISZERO (e1) -> (match (eval e1 env) with
    | Int n -> Bool (n=0)
    | _ -> raise(Failure "Type Error "))
  | LET (v,e1,e2) -> let ev = (eval e1 env) in (*First we evaluate value as is in the enviroment*)
    let ex = (extend_env (v,ev)) env in (eval e2 ex)  
  | LETREC (v1,v2, e1, e2) -> let ex1 = extend_env (v1,RecProcedure(v1,v2,e1,env)) env in (eval e2 ex1) (*First we extend the enviroment but use the Rec procedure which does include the name of the function to remember*)
  | PROC (v, e) -> Procedure(v, e ,env) (*The is no need to do nothing, but just build the procedure to be called later*)
  | CALL (e1, e2) -> (match (eval e1 env) with (*First we have to check which type of procedure is being utilized by evaluation to match*)
    | Procedure(v,bd, cenv) -> let av= eval e2 env in let eenv = (extend_env (v,av)) cenv in eval bd eenv(*Matched to a regular procedure, then do evaluate with the estructure*)
    | RecProcedure(v1, v2, bd, cenv) -> let av = eval e2 env in let updated_env = extend_env(v1, RecProcedure(v1,v2,bd,cenv)) cenv in
    let extended_env = extend_env(v2,av) updated_env in eval bd extended_env (*Many revision, eval-> extend -> extend and evaluate*)
    | _ -> raise(Failure "Type Error"))
  and binop op e1 e2 env = 
    match (eval e1 env, eval e2 env) with 
    | (Int n1, Int n2) -> Int (op n1 n2)
    | _ -> raise (Failure "Type Error")

(**************************************************************)
(********************** Example Programs **********************)
(**************************************************************)

(* let f = proc x (x+1) in f (f 1) *)
let pgm1 = 
  LET (
    "f", 
    PROC ("x", ADD (VAR "x", CONST 1)),
    CALL (VAR "f", CALL (VAR "f", CONST 1))
  )

(* letrec f = proc x (if iszero x then 0 else x + sum (x-1)) in sum 4  *)
let pgm2 = 
  LETREC (
    "sum", "x", 
      IF (ISZERO (VAR "x"), 
        CONST 0, 
        ADD (VAR "x", CALL (VAR "sum", SUB (VAR "x", CONST 1)))
      ),
      CALL (VAR "sum", CONST 4)
  )

(* letrec double = proc x (if iszero x then 0 else (double x-1) + 2) *)
let pgm3 = 
  LETREC ("double", "x", 
    IF (ISZERO (VAR "x"), 
      CONST 0, 
      ADD (CALL (VAR "double", SUB (VAR "x", CONST 1)), CONST 2)), 
    CALL (VAR "double", CONST 6))

let() =
  Printf.printf "--------------Output1-----------------------------------";;
  print_newline();;
let output_1 = eval pgm1 empty_env
let _ = print_value output_1
let() =
  Printf.printf "--------------Output2-----------------------------------";;
  print_newline();;
let output_2 = eval pgm2 empty_env
let _ = print_value output_2
let() =
  Printf.printf "--------------Output3-----------------------------------";;
  print_newline();;
let output_3 = eval pgm3 empty_env
let _ = print_value output_3

