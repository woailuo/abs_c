open Cil
open Uexception
module E = Errormsg

type funRecord = {mutable fName: string; mutable bType: string}

let funlist = ref (("", {fName=""; bType = ""} ):: [])

(* record the behavioral type *)
let recString = ref ""
let recFName = ref ""
		   
(* start from here:  behavioral abstraction *)
let rec  abstract (astfile : file ) : unit  =
  List.iter (fixGlobal) (astfile.globals)

 (*deal with a global, which is a function definition, from AST file*)
and  fixGlobal (glb: global ) : unit =
  match glb with
  | GFun (fd, loc) ->
     let fname = fd.svar.vname in
     recFName := fname;
     recString := "";
     print_string fname;  fixFunc fd;
     funlist := ((fname, {fName = fname; bType = !recString}) :: !funlist )
  | _ -> ()

(* deal with a function body, which contains a block*)
and  fixFunc (fbody: fundec) = fixBlock fbody.sbody 

(* deal with function body's blocks, which contain several statements*)
and fixBlock (fblock: block ) : unit =  fixStmts fblock.bstmts 

(* deal with statements*)
and fixStmts (stmlist: stmt list ) : unit = print_int (List.length stmlist) ;
					    print_newline ();
					    List.iter fixStmt stmlist

(* deal with a statement, which contains instructions of some sort*)
and fixStmt (stm: stmt)  = 
  match stm.skind with
  | Instr ilist -> print_string ( !recFName ^" instruction: "); print_int (List.length ilist) ; print_newline (); fixInstrs  ilist
  | Return (Some exp, loc) ->  print_string ( !recFName ^ " return-Some \n" ); fixExpr exp
  | Return (None, loc) -> print_string ( !recFName ^ " return-None \n" )
  | _ ->  recString := !recString ^ ""

(* deal with instructions*)
and fixInstrs ins : unit  = List.iter fixInstr ins

(*deal with one instruction *)
and fixInstr instr :unit  =
  match instr with
  | Set _ ->  recString := !recString  ^ " set "
  | _ -> recString := !recString ^ " other "

(*deal with expresion *)
and fixExpr (expr : exp) =
  match expr with
  |  _ ->  recString := !recString ^ " rset  "
  
and printFunlist funlist =
  match funlist with
    [] -> ()
  | (fname, record) :: tl ->
     print_string record.fName; print_newline ();
     print_string record.bType ; print_newline();
     printFunlist tl

(*main function calls abstract function *)									 let _ = Main.main (); abstract(!Main.astfile); printFunlist !funlist
