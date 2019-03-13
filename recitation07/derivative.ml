(** A small example of a program for computing derivatives of polynomials *)

(** Expressions for polynomials in one variable x *)
type expr = Num of float                 (* c *)
          | Var                          (* x *)
          | Mult of expr * expr          (* e1 * e2 *)
          | Add of expr * expr           (* e1 + e2 *)
          | Pow of expr * int            (* e1^n *)

(** Compute derivative [e'] of an expression [e] *)
let rec derivative = function
  | Num _ -> Num 0.0
  | Var -> Num 1.0
  | Add (e1, e2) -> Add (derivative e1, derivative e2)
  | Mult (e1, e2) -> Add (Mult (derivative e1, e2), Mult(e1, derivative e2))
  | Pow (e1, n) ->
      Mult (Mult (Num (float_of_int n), Pow (e1,  n - 1)),
            derivative e1)

let simplify_top = function
  | Mult (Num 0.0 as zero, _) | Mult(_, (Num 0.0 as zero)) -> zero
  | Mult (Num 1.0, e2) | Mult (e2, Num 1.0) -> e2
  | Mult (Num c1, Num c2) -> Num (c1 +. c2)
  | Add (Num 0.0, e2) | Add (e2, Num 0.0) -> e2
  | Add (Num c1, Num c2) -> Num (c1 *. c2)
  | e -> e

(** Simplify expression [e0] by applying [simplify_top] 
  * recursively until a fixpoint is reached *)
let rec simplify e0 =
  let e = simplify_top e0 in
  let e = match e with
  | Add (e1, e2) -> Add (simplify e1, simplify e2)
  | Mult (e1, e2) -> Mult (simplify e1, simplify e2)
  | Pow (e1, n) -> Pow (simplify e1, n)
  | e -> e
  in
  if e = e0 then e else simplify e

(** Compute derivative [e'] of an expression [e] 
  * and simplify [e'] *)
let derivative e = 
  e |> derivative |> simplify

(** Evaluate expression [e] with variable x mapped to [v] *)
let rec eval e v = match e with
  | Num c -> c
  | Var -> v
  | Add (e1, e2) -> eval e1 v +. eval e2 v
  | Mult (e1, e2) -> eval e1 v *. eval e2 v
  | Pow (e1, n) -> eval e1 v ** float_of_int n
