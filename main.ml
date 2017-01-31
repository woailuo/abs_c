module F = Frontc
module C = Cil
module E = Errormsg
open Uexception
open Cil

(* parsing a file. If file name is unvalid, raise error  *)
let  parseOneFile (fname: string) : C.file =
  try
    let cabs, cil =
      try
	F.parse_with_cabs fname ()
      with _ -> raise Err_of_main
    in
    Rmtmps.removeUnusedTemps cil;
    cil
  with
    _ -> raise Err_of_main

let filename = ref "Exprs/vip_file"

let astfile = ref (parseOneFile !filename )

let rec getFunbody g =
  match g with
  |  h :: tl ->
      begin
	match h with
	| GFun (fd, loc) -> fd
	| _ -> getFunbody tl
      end
	

let funbody = getFunbody !astfile.globals

(* main funciton  *)
let  rec main (): unit =
  C.print_CIL_Input := true;
  C.lineLength := 100000;
  C.warnTruncate := false;
  E.colorFlag := true;
  Cabs2cil.doCollapseCallCast := true;
  try
  let fname =  (* file name *)
       try
  	   Sys.argv.(1) (* get input file name from prompt. if no input_filename, raise error *)
      with
  	_ -> raise Err_of_file
  in
  filename := fname ;
  astfile :=  parseOneFile !filename
  with
  | Err_of_file -> print_string "Error at main.ml file, please input a file name \n"
  | Err_of_main  -> print_string " Error at main.ml file , please check the input file and input a valid file name \n"
  | Err_of_abs line  -> Printf.printf " Error at abs.ml file, %d  \n" line
  | _ -> print_string  "Error: there are some unknown errors \n"

