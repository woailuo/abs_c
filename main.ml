module F = Frontc
module C = Cil
module E = Errormsg

let parseOneFile (fname: string) : C.file =
  let cabs, cil = F.parse_with_cabs fname () in
  Rmtmps.removeUnusedTemps cil;
  cil

let  rec main (): unit =
  C.print_CIL_Input := true;
  C.lineLength := 100000;
  C.warnTruncate := false;
  E.colorFlag := true;
  Cabs2cil.doCollapseCallCast := true;
  try
    let fname = Sys.argv.(1) in (* file name *) 
    let file =  parseOneFile fname in
       try
	 Abs.abstract (file)
       with
	 _ -> print_string " Error : at the abs.ml file \n";
  with
      Invalid_argument str ->  print_string str;
    | _ -> print_string " Error: at main.ml file \n"

let _ = main ();;
