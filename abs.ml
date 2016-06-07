open Cil
open Uexception
module E = Errormsg

let printFlag = false	     
	     
type funRecord =
  {mutable fName: string; mutable bType: string; mutable funbody : fundec}

let funlist =
  ref (("", {fName=""; bType = ""; funbody = Main.funbody  } ):: [])

(* record the behavioral type *)
let recString = ref ""
let recFName = ref ""

let prints str  =
  if printFlag = true then print_string str
  else  ()
		   
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
     fixFunc fd;
     funlist := ((fname, {fName = fname; bType = !recString; funbody = fd }) :: !funlist );
     prints "fixGlobal : GFun End \n"
  | GAsm (str, loc) -> prints "fixGlobal : GAsm \n"
  | GPragma (attri, loc) -> prints "fixGlobal : GPragma \n"
  | GText str -> prints "fixGlobal : GTest \n"

(* deal with a function body, which contains a block*)
and  fixFunc (fbody: fundec) = fixBlock fbody.sbody 

(* deal with function body's blocks, which contain several statements*)
and fixBlock (fblock: block ) : unit = prints "fixBlock Start \n";
				       fixStmts fblock.bstmts ;
				        prints "fixBlock Start End \n"

(* deal with statements*)
and fixStmts (stmlist: stmt list ) : unit = List.iter fixStmt stmlist

(* deal with a statement, which contains instructions of some sort*)
and fixStmt (stm: stmt)  = 
  match stm.skind with
  | Instr ilist -> prints "fixStmt Instr Start \n" ;
		   fixInstrs  ilist;
		   prints "fixStmt Instr End\n"
  | Return (Some exp, loc) -> prints "fixStmt Return Start\n";
			      fixExpr exp;
			      prints "fixStmt Return End\n"
  | Return (refStmt, loc) -> prints "fixStmt Return \n"
  | Goto ( _ ,loc) -> prints "fixStmt Goto \n"
  | ComputedGoto (exp, loc) -> prints "fixStmt computeGoto Start \n";
			       fixExpr exp;
			       prints "fixStmt computeGoto End \n";
  | Break loc -> prints "fixStmt Break \n"
  | Continue loc -> prints "fixStmt Continue \n"
  | If (exp, tb, fb, loc ) -> prints "fixStmt If Start \n";
			      fixExpr exp;
			      let btypestr = !recString in
			      recString :="";
			      fixBlock tb;
			      let flag1 = isContainMF !recString in
			      let tbrec = !recString in
			      recString := ""; 
			      fixBlock fb;
			      let flag2 = isContainMF !recString in
			      let fbrec = !recString in
			      recString := btypestr ;
			      begin
				if (flag1 || flag2)
				then
				  begin
			       	    match flag1, flag2 with
				    | (true, true) ->
			     concatChars (fixLastCharacter !recString ) ("(" ^ tbrec ^","^fbrec ^")") 
				    | (true, false) ->
			     concatChars (fixLastCharacter !recString ) ("(" ^ tbrec ^","^ "0" ^")")
				    | (false, true) ->
			     concatChars (fixLastCharacter !recString ) ("(" ^ "0" ^","^ fbrec ^")")
				    | (false , false )-> ()
				  end
			      end; 
			      prints "fixStmt If End\n"
  | Switch (exp, blk, stmlist, loc) -> prints "fixStmt Switch Start \n";
				       fixExpr exp;
				       fixBlock blk;
				       (* fixStmts stmlist; *)
				       prints "fixStmt Switch End \n"
  | Loop (blk, loc, stmopt, stm2opt) -> prints "fixStmt Loop Start \n";
					fixBlock blk;
					 prints "fixStmt Loop  End\n"
  | Block blk -> prints "fixStmt block start \n";
		 fixBlock blk;
		 prints "fixStmt block end \n"
  | TryFinally (blk1, blk2, loc) -> prints "fixStmt TryFinally  Start\n";
				    fixBlock blk1;
				    fixBlock blk2;
				    prints "fixStmt TryFinally End \n"
  | TryExcept (blk1, (inslist, exp), blk2, loc) -> prints "fixStmt TryExcept Start \n";
						   fixBlock blk1;
						   fixExpr exp;
						   fixBlock blk2;
						   prints "fixStmt TryExcept End\n"

(* deal with instructions*)
and fixInstrs ins : unit  = List.iter fixInstr ins

(*deal with one instruction *)
and fixInstr instr :unit  =
  match instr with
  | Set (lv, exp, loc ) ->  prints "fixInstr set Start \n";
			    fixLval lv;
			    fixExpr exp;
			    prints "fixInstr set End \n"
  | Call (Some lv, exp, explist, loc) -> prints "fixInstr scall Start\n";
					 fixLval lv; fixExpr exp;
					 (prints " -list : " ; List.iter fixExpr explist);
					 prints " -list \n";
					 prints "fixInstr scall End \n"
  | Call (None, exp, explist, loc) -> prints "fixInstr ncall Start\n";
				      fixExpr exp;
				      List.iter fixExpr explist;
				      prints "fixInstr ncall End \n"
  | Asm _ -> prints " fixInstr Asm Start \n";
	     prints "fixInstr Asm End \n "

(*deal with Lval *)
and fixLval (lv : lval)  =
  match lv with
    (lhost, offset) ->  prints "fixLval Start \n" ; fixLhost lhost; fixOffset offset ;
			prints "fixLval End \n"

(*deal with Lval's lhost *)			 
and  fixLhost (lhost : lhost) =
  match lhost with 
  | Var varinfo -> prints (" fixLhost Var Start:  " ^ varinfo.vname ^ " \n");
		   
		   (* concatChars (fixLastCharacter !recString) varinfo.vname; *)
		   prints " fixLhost Var End \n"
  | Mem exp -> prints "fixLhost Mem Start\n";
	       fixExpr exp;
	       prints "fixLhost Mem End\n"

(*deal with Lval's offset *)
and fixOffset (offset : offset) =
  match offset with
  | NoOffset ->  prints "fixOffset nooffset \n"
  | Field (filedinfo, offset) ->  prints "fixOffset Field Start \n";
				  fixOffset offset;
				  prints "fixOffset Field End \n"
  | Index (exp, offset) ->  prints "fixOffset Index Start \n";
			    fixExpr exp;
			    fixOffset offset ;
			    prints "fixOffset Index End\n" 

(* get the name of lval *)
(* and  fixValofLval (lv: lval) = *)
(*   match lv with *)
(*   | (Var varinfo, offset) ->  varinfo.vname *)
(*   | _ -> "" *)

(*deal with expresion *)
and fixExpr (expr : exp) =
  match expr with
  | Const v -> prints "fixExpr Const v\n"
  | Lval lval -> prints "fixExpr Lval Start\n";
		 fixLval lval ;
		 prints " fixExpr Lval End\n"
  | SizeOf typ -> prints "fixExpr sizeof v\n"
  | SizeOfE exp -> prints "fixExpr sizeofE Start \n";
		   fixExpr exp;
		   prints "fixExpr sizeofE End \n";
  | SizeOfStr strs -> prints "fixExpr sizeofstr \n"
  | AlignOf typ -> prints "fixExpr Alignof \n"
  | AlignOfE exp -> prints "fixExpr AlignofE Start \n";
		    fixExpr exp;
		    prints "fixExpr AlignofE End \n"
  | UnOp (op, exp, typ) -> prints "fixExpr UnOp Start  \n";
			   fixExpr exp;
			   prints "fixExpr UnOp End \n"
  | BinOp (bop, exp1, exp2, typ) -> prints "fixExpr Binop Start\n";
				    fixExpr exp1;
				    fixExpr exp2;
				    prints "fixExpr Binop End\n"
  | Question (exp1, exp2, exp3, typ) -> prints "fixExpr Question Start\n";
					fixExpr exp1;
					fixExpr exp2;
					fixExpr exp3;
					prints "fixExpr Question End\n"
  | CastE (typ, exp) -> prints "fixExpr  CastE Start\n";
			fixExpr exp;
			prints "fixExpr  CastE End \n"
  | AddrOf lval -> prints "fixExpr Addrof Start\n";
		   fixLval lval ;
		   prints "fixExpr Addrof End \n"
  | AddrOfLabel ref_stmt -> prints "fixExpr AddrofLabel\n"
  | StartOf lval -> prints "fixExpr StartOf Start\n";
		    fixLval lval;
		    prints "fixExpr StartOf End\n"
		    

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
  if (varinfo = "malloc" || varinfo = "free" || (String.contains varinfo '(')) then
    begin
      match str with
	")" -> recString := !recString ^ ";" ^ varinfo 
      | "(" ->  recString := !recString ^ varinfo 
      | ";" ->  recString := !recString ^ varinfo
      | "c"| "e" -> recString := !recString ^ ";" ^ varinfo
      | _ -> recString :=  varinfo
    end
  else
    ()

and isContainMF str =
  let bm = String.contains str 'm'  in
  let bf = String.contains str 'f' in
  bm || bf
    
  
(*main function calls abstract function *)
let main () = Main.main (); abstract(!Main.astfile)
