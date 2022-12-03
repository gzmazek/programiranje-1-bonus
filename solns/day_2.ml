let load vsebina_datoteke =
  let s = String.split_on_char '\n' vsebina_datoteke in
  s |> List.map String.trim

let value = function
   | 'X' -> 1
   | 'Y' -> 2
   | 'Z' -> 3
   | _ -> failwith "Ni pravilne vrednsti moje poteze"

let spremeni_prve str = match str with
  | 'A' -> 'X'
  | 'B' -> 'Y'
  | 'C' -> 'Z'
  | _ -> failwith "Ni pravilne vrednsti njegove poteze"

let ciklicno = function
  | 'X' -> 'Y'
  | 'Y' -> 'Z'
  | 'Z' -> 'X'
  | _ -> failwith "Ni pravilne vrednsti njegove poteze"

let explode str = (*Funkcija ni moja, StackOverflow*)
  let rec exp a b =
    if a < 0 then b
    else exp (a - 1) (str.[a] :: b)
  in
  exp (String.length str - 1) []

let evaluate sez =
  let player0 = List.nth (explode sez) 0 in
  let player2 = List.nth (explode sez) 2 in
  let player1 = ciklicno (spremeni_prve player0) in
  if (spremeni_prve player0) = player2 then (3 + value player2) 
    else if player1 = player2 then (6 + value player2) 
    else (value player2)

let rec sestej acc = function
  | [] -> acc
  | a :: sez -> sestej (acc + evaluate a) sez
 
let naloga1 vsebina_datoteke =
    let sez = load vsebina_datoteke in
    string_of_int (sestej 0 sez)


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
  let vsebina_datoteke = preberi_datoteko ("input/day_2.in") in

  let p1_start = Sys.time () in
  let odgovor1 = naloga1 vsebina_datoteke in
  let t1_time = Sys.time () -. p1_start in

  let p2_start = Sys.time () in
  let odgovor2 = naloga1 vsebina_datoteke in
  let t2_time = Sys.time () -. p2_start in

  print_endline ("P1 taken: " ^ string_of_float t1_time ^ "s");
  print_endline ("P2 taken: " ^ string_of_float t2_time ^ "s");
  print_endline ("Total: " ^ string_of_float (t1_time +. t2_time) ^ "s");

  izpisi_datoteko ("output/day_2_1.out") odgovor1;
  izpisi_datoteko ("output/day_2_2.out") odgovor2
