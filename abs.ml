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
  | GType (typeinfo, loc) -> print_string "fixGlobal Gtype \n"
  | GCompTag (compinfo, loc) -> print_string "fixGlobal : GCompTag \n"
  | GCompTagDecl (compinfo, loc) -> print_string "fixGlobal : GCompTagDecl \n"
  | GEnumTag (enuminfo, loc) -> print_string "fixGlobal : GEnumTag \n"
  | GEnumTagDecl (enuminfo, loc) -> print_string "fixGlobal : GCompTag \n"
  | GVarDecl (varinfo, loc) -> print_string "fixGlobal : GVarDecl \n"
  | GVar (varinfo, initinfo, loc) -> print_string "fixGlobal : Gvar \n"
  | GFun (fd, loc) ->
     print_string "fixGlobal : GFun Start \n";
     let fname = fd.svar.vname in
     recFName := fname;
     recString := "";
     fixFunc fd;
     funlist := ((fname, {fName = fname; bType = !recString}) :: !funlist );
     print_string "fixGlobal : GFun End \n"
  | GAsm (str, loc) -> print_string "fixGlobal : GAsm \n"
  | GPragma (attri, loc) -> print_string "fixGlobal : GPragma \n"
  | GText str -> print_string "fixGlobal : GTest \n"

(* deal with a function body, which contains a block*)
and  fixFunc (fbody: fundec) = fixBlock fbody.sbody 

(* deal with function body's blocks, which contain several statements*)
and fixBlock (fblock: block ) : unit = print_string "fixBlock Start \n";
				       fixStmts fblock.bstmts ;
				        print_string "fixBlock Start End \n"

(* deal with statements*)
and fixStmts (stmlist: stmt list ) : unit = List.iter fixStmt stmlist

(* deal with a statement, which contains instructions of some sort*)
and fixStmt (stm: stmt)  = 
  match stm.skind with
  | Instr ilist -> print_string "fixStmt Instr Start \n" ;
		   fixInstrs  ilist;
		   print_string "fixStmt Instr End\n"
  | Return (Some exp, loc) -> print_string "fixStmt Return Start\n";
			      fixExpr exp;
			      print_string "fixStmt Return End\n"
  | Return (refStmt, loc) -> print_string "fixStmt Return \n"
  | Goto ( _ ,loc) -> print_string "fixStmt Goto \n"
  | ComputedGoto (exp, loc) -> print_string "fixStmt computeGoto \n"
  | Break loc -> print_string "fixStmt Break \n"
  | Continue loc -> print_string "fixStmt Continue \n"
  | If (exp, tb, fb, loc ) -> print_string "fixStmt If \n"
  | Switch (exp, blk, stmlist, loc) -> print_string "fixStmt Switch \n"
  | Loop (blk, loc, stmopt, stm2opt) -> print_string "fixStmt Loop \n"
  | Block blk -> print_string "fixStmt block start \n";
		 fixBlock blk;
		 print_string "fixStmt block end \n"
  | TryFinally (blk1, blk2, loc) -> print_string "fixStmt TryFinally \n"
  | TryExcept (blk1, (inslist, exp), blk2, loc) -> print_string "fixStmt TryExcept \n"
				      
(* deal with instructions*)
and fixInstrs ins : unit  = List.iter fixInstr ins

(*deal with one instruction *)
and fixInstr instr :unit  =
  match instr with
  | Set (lv, exp, loc ) ->  print_string "fixInstr set Start \n";
			    fixLval lv;
			    fixExpr exp;
			    print_string "fixInstr set End \n"
  | Call (Some lv, exp, explist, loc) -> print_string "fixInstr scall Start\n";
					 fixLval lv; fixExpr exp;
					 (print_string " -list : " ; List.iter fixExpr explist);
					 print_string " -list \n";
					 print_string "fixInstr scall End \n"
  | Call (None, exp, explist, loc) -> print_string "fixInstr ncall Start\n";
				      fixExpr exp;
				      List.iter fixExpr explist;
				      print_string "fixInstr ncall End \n"
  | Asm _ -> print_string " fixInstr Asm Start \n";
	     print_string "fixInstr Asm End \n "

(*deal with Lval *)
and fixLval (lv : lval)  =
  match lv with
    (lhost, offset) ->  print_string "fixLval Start \n" ; fixLhost lhost; fixOffset offset ;
			print_string "fixLval End \n"

(*deal with Lval's lhost *)			 
and  fixLhost (lhost : lhost) =
  match lhost with 
  | Var varinfo -> print_string (" fixLhost Var Start:  " ^ varinfo.vname ^ " \n");
		   concatChars !recString varinfo.vname;
		   print_string " fixLhost Var End \n"
  | Mem exp -> print_string "fixLhost Mem Start\n";
	       fixExpr exp;
	       print_string "fixLhost Mem End\n"

(*deal with Lval's offset *)
and fixOffset (offset : offset) =
  match offset with
  | NoOffset ->  print_string "fixOffset nooffset \n"
  | Field (filedinfo, offset) ->  print_string "fixOffset Field Start \n";
				  fixOffset offset;
				  print_string "fixOffset Field End \n"
  | Index (exp, offset) ->  print_string "fixOffset Index Start \n";
			    fixExpr exp;
			    fixOffset offset ;
			    print_string "fixOffset Index End\n" 

(* get the name of lval *)
(* and  fixValofLval (lv: lval) = *)
(*   match lv with *)
(*   | (Var varinfo, offset) ->  varinfo.vname *)
(*   | _ -> "" *)

(*deal with expresion *)
and fixExpr (expr : exp) =
  match expr with
  | Const v -> print_string "fixExpr Const v\n"
  | Lval lval -> print_string "fixExpr Lval Start\n";
		 fixLval lval ;
		 print_string " fixExpr Lval End\n"
  | SizeOf typ -> print_string "fixExpr sizeof v\n"
  | SizeOfE exp -> print_string "fixExpr sizeofe v\n"
  | SizeOfStr strs -> print_string "fixExpr sizeofstr \n"
  | AlignOf typ -> print_string "fixExpr Alignof \n"
  | AlignOfE exp -> print_string "fixExpr AlignofE \n"
  | UnOp (op, exp, typ) -> print_string "fixExpr UnOp \n"
  | BinOp (bop, exp1, exp2, typ) -> print_string "fixExpr Binop v\n"
  | Question (exp1, exp2, exp3, typ) -> print_string "fixExpr Question v\n"
  | CastE (typ, exp) -> print_string "fixExpr  CastE\n"
  | AddrOf lval -> print_string "fixExpr Addrof \n"
  | AddrOfLabel ref_stmt -> print_string "fixExpr AddrofLabel\n"
  | StartOf lval -> print_string "fixExpr StartOf v\n"

(* and fixExprName ( expr: exp) = *)
(*   match expr with *)
(*   | Lval (Var v, offset) ->  *)
(*   | Lval (Mem exp, offset) ->  *)
(*   | _ ->  *)

(* get the last character *)
and fixLastCharacter (strs : string) : string =
  let len  = String.length strs in
  if (len > 0) then
    (String.make 1 strs.[len-1])
  else 
    !recString
     
and  concatChars str  varinfo =
  match varinfo with
  | "malloc" | "free" ->
    begin
      match str with
	")" -> recString := !recString ^ ";" ^ varinfo 
      | "(" ->  recString := !recString ^ varinfo 
      | ";" ->  recString := !recString ^ varinfo
      | "c"| "e" -> recString := !recString ^ ";" ^ varinfo
      | _ -> recString :=  varinfo
    end
  | _ -> ()
    
and printFunlist funlist =
  match funlist with
    [] -> ()
  | (fname, record) :: tl when fname <> "" ->
     print_string (record.fName ^ "() : "); print_newline ();
     print_string ( "   " ^ record.bType ); print_newline();
     printFunlist tl;
  | _ -> ()
	   
(*main function calls abstract function *)
let _ = Main.main (); abstract(!Main.astfile);
	print_string "\n-----------------------------------------------\n";
	printFunlist !funlist;
	print_string "-----------------------------------------------\n";
