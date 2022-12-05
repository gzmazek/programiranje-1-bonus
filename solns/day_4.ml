let load vsebina_datoteke =
  let s = String.split_on_char '\n' vsebina_datoteke in
  s |> List.map String.trim 

let predelaj vrstica = (String.split_on_char ',' vrstica) |> List.map (String.split_on_char '-')

let determine vrstica = let [[x1; x2]; [y1; y2]] = predelaj vrstica in
    (int_of_string x1 - int_of_string y1) * (int_of_string y2 - int_of_string x2) >= 0

let stevilo_v_preseku vrstica = let [[x1; x2]; [y1; y2]] = predelaj vrstica in
    (Int.abs ((max (int_of_string x1) (int_of_string x2)) - (min (int_of_string y1) (int_of_string y2)))) + 1

(*ta int_of_string je grozen, ni se mi dalo pol krajsat kode*)

let rec sestej acc = function
  | [] -> acc
  | x :: xs -> if determine x then 
                sestej (acc + (stevilo_v_preseku x)) xs
              else sestej acc xs

let rec count acc = function
  | [] -> acc
  | x :: xs -> if determine x
              then count (acc + 1) xs
              else count acc xs


let naloga1 vsebina_datoteke =
    let sez = load vsebina_datoteke in
    string_of_int (count 0 sez)
    
let naloga2 vsebina_datoteke =
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
  let vsebina_datoteke = preberi_datoteko ("input/day_4.in") in

  let p1_start = Sys.time () in
  let odgovor1 = naloga1 vsebina_datoteke in
  let t1_time = Sys.time () -. p1_start in

  let p2_start = Sys.time () in
  let odgovor2 = naloga2 vsebina_datoteke in
  let t2_time = Sys.time () -. p2_start in

  print_endline ("P1 taken: " ^ string_of_float t1_time ^ "s");
  print_endline ("P2 taken: " ^ string_of_float t2_time ^ "s");
  print_endline ("Total: " ^ string_of_float (t1_time +. t2_time) ^ "s");

  izpisi_datoteko ("output/day_4_1.out") odgovor1;
  izpisi_datoteko ("output/day_4_2.out") odgovor2
