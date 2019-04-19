open Arithmetic

let n = 12345689123456789
let d = 98765432198654321

let rn = BigPosRat.(from_int (n,d))

let n' = 5
let d' = 6
let rn' = BigPosRat.(from_int (n',d'))

let _ =
  print_string BigPosRat.(rn |> to_string);
  print_newline ();
  print_string BigPosRat.(rn - rn |> to_string);
  print_newline ();
  print_string BigPosRat.(from_int (2,1) |> ( * ) rn' |> to_string);
  print_newline ();
  print_string BigPosRat.(rn' + rn' + rn' |> to_string);
  print_newline ();