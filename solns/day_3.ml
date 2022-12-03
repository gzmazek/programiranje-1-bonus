let load vsebina_datoteke =
  let s = String.split_on_char '\n' vsebina_datoteke in
  s |> List.map String.trim

let rec sum acc = function
  | [] -> acc
  | vrs :: sez -> sum (acc + (evaluate vrs)) sez

let prva_polovica vrstica = sub vrstica 0 ((lenght vrstica) / 2)

let druga_polovica vrstica = ()

let evaluate vrstica = ()

let naloga1 vsebina_datoteke =
    let sez = load vsebina_datoteke in
    string_of_int (max_cal 0 sez)
    
let naloga2 vsebina_datoteke =
    let sez = load vsebina_datoteke in
    string_of_int (max_cal_3 (0, 0, 0) sez)

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
