(** Signature of module providing arbitrary precision arithmetic on natural numbers *)
module type BigNatType =
  sig
    type bignat

    (** The number 0 **)
    val zero: bignat

    (** The number 1 **)
    val one: bignat

    (** Convert an int to a big natural *)
    val from_int : int -> bignat

    (** Convert a big natural to an int (may overflow) *)
    val to_int : bignat -> int

    (** Convert a big naturals to a string in decimal representation *)
    val to_string : bignat -> string

    (** Addition of big naturals *)
    val (+) : bignat -> bignat -> bignat

    (** Subtraction of big naturals. Raises [Invalid_argument] if result is negative. *)
    val (-) : bignat -> bignat -> bignat

    (** Multiplication of big naturals *)        
    val ( * ) : bignat -> bignat -> bignat

  end

module BigNat : BigNatType =
  struct
    type bignat = int list (* least significant first *)

    let base = 10000 (* must satisfy: float_of_int base <= sqrt (float_of_int max_int) *)

    let zero = []

    let one = [1]
    
    let rec from_int = function
      | 0 -> []
      | n ->
          if n < 0 then raise (Invalid_argument "negative number")
          else n mod base :: from_int (n / base)

    let rec to_int = function
      | [] -> 0
      | d :: ds -> d + base * to_int ds (* may overflow *)

    let rec pad =
      let base_len = String.length (string_of_int base) - 1 in
      fun s -> if String.length s = base_len then s else pad ("0" ^ s)

    let to_string ds =
      match List.rev ds with
      | [] -> "0"
      | d :: dr ->
          string_of_int d :: List.map (fun d -> d |> string_of_int |> pad) dr |>
          String.concat ""

    (* like :: on type int list but drops leading 0s *)
    let zcons d ds =
      match d, ds with
      | 0, []-> []
      | _ -> d :: ds 

    let rec add ar br c =
      match ar, br, c with
      | ar, [], 0 -> ar
      | [], br, 0 -> br
      | [], [], c -> [c]
      | ar, [], c -> add ar [0] c
      | [], br, c -> add [0] br c
      | a :: ar', b :: br', c ->
          let d, c' =
            if a + b + c < base
            then a + b + c, 0    
            else a + b + c - base, 1      
          in
          d :: add ar' br' c'
                
    let rec sub ar br c =
      match ar, br, c with
      | _, [], 0 -> ar
      | [], br, c -> raise (Invalid_argument "result is negative")
      | ar, [], c -> sub ar [0] c
      | a :: ar', b :: br', c ->
          let d, c' =
            if a - b - c >= 0
            then a - b - c, 0
            else a - b - c + base, 1
          in
          zcons d (sub ar' br' c')
            
            
    let (+) x y = add x y 0
    let (-) x y = sub x y 0

    let rec mul ar b =
      match ar, b with
      | _, 0 
      | [], _ -> []
      | a :: ar, b ->
          from_int (a * b) + (0 :: mul ar b)

    let rec ( * ) x br =
      match x, br with
      | [], _ 
      | _, [] -> []
      | x, b :: br ->
          mul x b + (0 :: (x * br))

  end

(** Signature of module providing arbitrary precision arithmetic on positive rationals *)
module type BigPosRatType =
  sig
    type bigposrat 

    (** The number 0 *)
    val zero: bigposrat 

    (** The number 1 *)
    val one: bigposrat

     (** Convert an int to a big positive natural *)
    val from_int : int * int -> bigposrat

    (** Convert a big positive rational to an int pair (may overflow) *)
    val to_int : bigposrat -> int * int

    (** Convert a big positive rational to a string in decimal / decimal representation *)
    val to_string : bigposrat -> string

    (** Addition of big positive rationals *)
    val (+) : bigposrat -> bigposrat -> bigposrat

    (** Subtraction of big positive rationals. Raises [Invalid_argument] if result is negative. *)
    val (-) : bigposrat -> bigposrat -> bigposrat

    (** Multiplication of big positive rationals *)        
    val ( * ) : bigposrat -> bigposrat -> bigposrat

  end

module MakeBigPosRat(BigNat : BigNatType) : BigPosRatType =
  struct
    type bigposrat = BigNat.bignat * BigNat.bignat

    (* Note that the denominator cannot be zero *)
    let zero = BigNat.zero, BigNat.one

    (* 1 / 1 *)
    let one = BigNat.one, BigNat.one

    let from_int (x,y) =
      if y = 0
      then raise (Invalid_argument "denominator cannot be zero")
      else BigNat.from_int x, BigNat.from_int y

    (* Caution: may overflow *)
    let to_int (n,d) = BigNat.to_int n, BigNat.to_int d

    let to_string (n,d) =
        BigNat.to_string n ^ " / " ^ BigNat.to_string d

    let (+) r1 r2 =
      match r1, r2 with
      | (n1, d1), (n2, d2) ->
          if d1 = d2 then BigNat.(n1 + n2), d1
          else
            let n1', n2' = BigNat.(n1 * d2), BigNat.(n2 * d1) in 
            let d' = BigNat.(d1 * d2) in 
            BigNat.(n1' + n2'), d'

    (** Will raise an exception if we get a negative value.
      * Can you see why?
      * It is because BigNat will raise an exception. *)
    let (-) r1 r2 =
      match r1, r2 with
      | (n1, d1), (n2, d2) ->
          if d1 = d2 then BigNat.(n1 - n2), d1
          else
            let n1', n2' = BigNat.(n1 * d2), BigNat.(n2 * d1) in 
            let d' = BigNat.(d1 * d2) in 
            BigNat.(n1' - n2'), d'

    let ( * ) r1 r2 =
      match r1, r2 with
      | (n1, d1), (n2, d2) ->
          BigNat.(n1 * n2), BigNat.(d1 * d2)

  end

module BigPosRat = MakeBigPosRat(BigNat)