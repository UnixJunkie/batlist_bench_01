open Benchmark
open Printf

module L = BatList

let favg_opt l =
  match l with
  | [] -> assert(false)
  | x::xs ->
    let acc = ref x in
    let len = ref 1 in
    let rem = ref xs in
    let go = ref true in
    while !go do
      match !rem with
      | [] -> go := false;
      | x::xs ->
        acc := !acc +. x;
        incr len;
        rem := xs
    done;
    !acc /. float_of_int !len

let fsum_opt l =
  match l with
  | [] -> assert(false)
  | x::xs ->
    let acc = ref x in
    let rem = ref xs in
    let go = ref true in
    while !go do
      match !rem with
      | [] -> go := false;
      | x::xs ->
        acc := !acc +. x;
        rem := xs
    done;
    !acc

let favg_naive l =
  let rec loop (sumf, n) = function
    | [] -> sumf /. (float_of_int n)
    | x :: xs -> loop (sumf +. x, n + 1) xs
  in
  if l = [] then assert(false)
  else loop (0.0, 0) l

let fsum_naive l =
  let rec loop sumf = function
    | [] -> sumf
    | x :: xs -> loop (sumf +. x) xs
  in
  if l = [] then assert(false)
  else loop 0.0 l

let () =
  assert(favg_naive [1.;2.;3.] = favg_opt [1.;2.;3.]);
  assert(favg_naive [1.;2.;3.] = 2.);
  assert(fsum_naive [1.;2.;3.] = fsum_opt [1.;2.;3.]);
  assert(fsum_naive [1.;2.;3.] = 6.);
  let sizes =
    [1; 2; 4; 8; 16; 32; 64; 128; 256; 512; 1024; 2048; 4096; 8192; 16384;
     32768; 65536; 131072; 262144; 524288; 1048576] in
  let floats_array = Array.make 1048576 0.0 in
  for i = 0 to 1048576 - 1 do
    floats_array.(i) <- Random.float 1.0
  done;
  let floats_list = Array.to_list floats_array in
  let float_lists =
    L.map (fun n ->
        L.take n floats_list
      ) sizes in
  let many_favg_naive lists =
    L.iter (fun l ->
        ignore(favg_naive l)
      ) lists in
  let many_fsum_naive lists =
    L.iter (fun l ->
        ignore(fsum_naive l)
      ) lists in
  let many_favg_opt lists =
    L.iter (fun l ->
        ignore(favg_opt l)
      ) lists in
  let many_fsum_opt lists =
    L.iter (fun l ->
        ignore(fsum_opt l)
      ) lists in
  let res0 = throughputN ~repeat:5 1
      [("favg_opt",   many_favg_opt,   float_lists);
       ("favg_naive", many_favg_naive, float_lists)] in
  tabulate res0;
  let res1 = throughputN ~repeat:5 1
      [("fsum_opt",   many_fsum_opt,   float_lists);
       ("fsum_naive", many_fsum_naive, float_lists)] in
  tabulate res1
