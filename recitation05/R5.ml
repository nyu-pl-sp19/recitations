let rec product xs = match xs with
  | [] -> 1
  | hd :: tl -> hd * product tl


let product_better xs =
  let rec product_tr xs acc = match xs with
  | [] -> 0
  | [a] -> a * acc
  | hd :: tl -> product_tr tl (hd * acc)
  in
  product_tr xs 1

let positive_product xs =
  let rec product_tr acc = function
  | [] -> 0
  | [a] -> if a > 0 then a * acc else acc
  | hd :: tl -> if hd > 0 then product_tr (hd * acc) tl else product_tr acc tl
  in
  product_tr 1 xs

let positive_product_gaurd xs =
  let rec product_tr acc = function
  | [] -> 0
  | [a] when a > 0 -> a * acc
  | [a] when a < 0 -> acc
  | hd :: tl when hd > 0 -> product_tr (hd * acc) tl
  | _ :: tl -> product_tr acc tl
  in
  product_tr 1 xs