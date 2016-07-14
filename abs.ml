open Cil
open Utype
open Uexception
module E = Errormsg
module Ret = Oneret

let funlist =
  ref (("", {fName=""; bType = ""; funbody = Main.funbody  } ):: [])

(* record the behavioral type *)
let recString = ref ""
let recFName = ref ""

(* start from here:  behavioral abstraction *)
let rec  abstract (astfile : file ) : unit  =
  List.iter (fixGlobal) (astfile.globals)

 (*deal with a global, which is a function definition, from AST file*)
and  fixGlobal (glb: global ) : unit =
  match glb with
  | GType (typeinfo, loc) -> prints "fixGlobal Gtype \n"
  | GCompTag (compinfo, loc) -> prints "fixGlobal : GCompTag \n"
  | GCompTagDecl (compinfo, loc) -> prints "fixGlobal : GCompTagDecl \n"
  | GEnumTag (enuminfo, loc) -> prints "fixGlobal : GEnumTag \n"
  | GEnumTagDecl (enuminfo, loc) ->prints "fixGlobal : GCompTag \n"
  | GVarDecl (varinfo, loc) ->  prints "fixGlobal : GVarDecl \n"
  | GVar (varinfo, initinfo, loc) -> prints "fixGlobal : Gvar \n"
  | GFun (fd, loc) ->
    prints "fixGlobal : GFun Start \n";
     let fname = fd.svar.vname in
     recFName := fname;
     recString := "";
     (* fixFunc fd; *)
     Ret.oneret fd;
     (* prepareCFG fd; *)
     funlist := (("%"^fname^"%", {fName = "%"^fname^"%"; bType = !recString; funbody = fd }) :: !funlist );
     prints "fixGlobal : GFun End \n"
  | GAsm (str, loc) -> prints "fixGlobal : GAsm \n"
  | GPragma (attri, loc) -> prints "fixGlobal : GPragma \n"
  | GText str -> prints "fixGlobal : GTest \n"

(*main function calls abstract function *)
let main () = Main.main ();

  (  let channel = open_out "rewritten_file1.c" in
     (dumpFile (!printerForMaincil) channel "rewritten_file1.c") !Main.astfile;
     close_out channel );

  Cfg.computeFileCFG(!Main.astfile);

  abstract(!Main.astfile);

  ( let channel2 = open_out "rewritten_file2.c" in
     (dumpFile (!printerForMaincil) channel2 "rewritten_file2.c") !Main.astfile;
     close_out channel2)
