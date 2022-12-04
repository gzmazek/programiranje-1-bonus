let load vsebina_datoteke =
  let s = String.split_on_char '\n' vsebina_datoteke in
  s |> List.map String.trim

let explode str = (*Funkcija ni moja, StackOverflow*)
  let rec exp a b =
    if a < 0 then b
    else exp (a - 1) (str.[a] :: b)
  in
  exp (String.length str - 1) []

let prva_polovica_seznam vrstica = explode (String.sub vrstica 0 ((String.length vrstica) / 2))

let druga_polovica vrstica = String.sub vrstica ((String.length vrstica) / 2) ((String.length vrstica) / 2)

let rec element_v_preseku konec = function
  | [] -> failwith "Nekje naj ne bi bilo preseka"
  | a :: sez -> 
    if String.contains konec a
    then a
    else element_v_preseku konec sez

let rec element_v_preseku_2 konec1 konec2 = function
  | a :: sez -> 
    if (String.contains konec1 a) && (String.contains konec2 a)
    then a
    else element_v_preseku_2 konec1 konec2 sez
  | _ -> failwith "Element v preseku 2 necesa ni matchal!"

let presek vrstica = element_v_preseku (druga_polovica vrstica) (prva_polovica_seznam vrstica)

let abeceda = "_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

let evaluate crka = StringLabels.index abeceda crka

let rec sum acc = function
  | [] -> acc
  | vrs :: sez -> sum (acc + (evaluate (presek vrs))) sez

let rec sum_2 acc = function
    | x1 :: x2 :: x3 :: sez -> sum_2 (acc + evaluate (element_v_preseku_2 x2 x3 (explode x1))) sez
    | _ -> acc

let naloga1 vsebina_datoteke =
    let sez = load vsebina_datoteke in
    string_of_int (sum 0 sez)
    
let naloga2 vsebina_datoteke =
    let sez = load vsebina_datoteke in
    string_of_int (sum_2 0 sez)

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
  let vsebina_datoteke = preberi_datoteko ("input/day_3.in") in

  let p1_start = Sys.time () in
  let odgovor1 = naloga1 vsebina_datoteke in
  let t1_time = Sys.time () -. p1_start in

  let p2_start = Sys.time () in
  let odgovor2 = naloga2 vsebina_datoteke in
  let t2_time = Sys.time () -. p2_start in

  print_endline ("P1 taken: " ^ string_of_float t1_time ^ "s");
  print_endline ("P2 taken: " ^ string_of_float t2_time ^ "s");
  print_endline ("Total: " ^ string_of_float (t1_time +. t2_time) ^ "s");

  izpisi_datoteko ("output/day_3_1.out") odgovor1;
  izpisi_datoteko ("output/day_3_2.out") odgovor2
