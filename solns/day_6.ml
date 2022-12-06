(****************************************************)
let explode str = (*Funkcija ni moja, StackOverflow*)
  let rec exp a b =
    if a < 0 then b
    else exp (a - 1) (str.[a] :: b)
  in
  exp (String.length str - 1) []

let rec exist elem lst =
  match lst with
  | [] -> false
  | hd :: tl -> Char.equal elem hd || exist elem tl

let rec dupExist lst =
  match lst with
  | [] -> false
  | hd :: tl -> (exist hd tl) || dupExist tl

let rec firstk k xs = match xs with
  | [] -> failwith "firstk"
  | x::xs -> if k=1 then [x] else x::firstk (k-1) xs
(****************************************************)

let rec preveri i = function
  | a :: b :: c :: d :: sez -> let zac = [a; b; c; d] in
    if Bool.not (dupExist zac) then
      i
    else preveri (i + 1) (b :: c :: d :: sez)
  | [] -> failwith "Prekratek seznam"
  | _ -> failwith "Nekaj drugega je narobe"

let rec preveri_14 i = function
  | a :: sez -> let zac = a :: (firstk 13 sez) in
    if Bool.not (dupExist zac) then
      i
    else preveri_14 (i + 1) sez
  | [] -> failwith "Prekratek seznam"
  | _ -> failwith "Nekaj drugega je narobe"

let naloga1 vsebina_datoteke =
    let sez = explode vsebina_datoteke in
    string_of_int (preveri 4 sez)
    
let naloga2 vsebina_datoteke =
    let sez = explode vsebina_datoteke in
    string_of_int (preveri_14 14 sez)

let _ =
  let preberi_datoteko ime_datoteke =
      let chan = open_in_bin ime_datoteke in
      let vsebina = really_input_string chan (in_channel_length chan) in
      close_in chan;
      vsebina
  and izpisi_datoteko ime_datoteke vsebina =
      let chan = open_out_bin ime_datoteke in
      output_string chan vsebina;
      close_out chan
  in
  let vsebina_datoteke = preberi_datoteko ("input/day_6.in") in

  let p1_start = Sys.time () in
  let odgovor1 = naloga1 vsebina_datoteke in
  let t1_time = Sys.time () -. p1_start in

  let p2_start = Sys.time () in
  let odgovor2 = naloga2 vsebina_datoteke in
  let t2_time = Sys.time () -. p2_start in

  print_endline ("P1 taken: " ^ string_of_float t1_time ^ "s");
  print_endline ("P2 taken: " ^ string_of_float t2_time ^ "s");
  print_endline ("Total: " ^ string_of_float (t1_time +. t2_time) ^ "s");

  izpisi_datoteko ("output/day_6_1.out") odgovor1;
  izpisi_datoteko ("output/day_6_2.out") odgovor2