open Cil
open Uexception
module E = Errormsg
open Abs

let funslist = Abs.funlist

let bhtString = ref ""
let funcName = ref ""

(* type funRecord = *)
(*   {mutable fName: string; mutable bType: string; mutable funbody : fundec} *)

(* let funlist = *)
(*   ref (("", {fName=""; bType = ""; funbody = Main.funbody  } ):: []) *)
		 
(* print behavioral types of functions*)
let rec  printFunlist funlist =
  match funlist with
    [] -> ()
  | (fname, record) :: tl when fname <> "" ->
     print_string (record.fName ^ "() : "); print_newline ();
     print_string  ( "   " ^ record.bType ); print_newline();
     printFunlist tl;
  | _ -> ()

and comBeh funlist = List.iter comFun !funlist

and  comFun frecord =
  match frecord with
  | (fname, record) when fname <> "" -> comFunBody record.funbody
  | _ -> ()

and  comFunBody fbody =  comBlock fbody.sbody

and comBlock fblock = List.iter comStmt fblock.bstmts

and comStmt (stm: stmt)  = 
  match stm.skind with
  | Instr ilist -> prints "comStmt Instr Start \n" ;
		   comInstrs  ilist;
		   prints "comStmt Instr End\n"
  | Return (Some exp, loc) -> prints "comStmt Return Start\n";
			      comExpr exp;
			      prints "comStmt Return End\n"
  | Return (refStmt, loc) -> prints "comStmt Return \n"
  | Goto ( _ ,loc) -> prints "comStmt Goto \n"
  | ComputedGoto (exp, loc) -> prints "comStmt computeGoto Start \n";
			       comExpr exp;
			       prints "comStmt computeGoto End \n";
  | Break loc -> prints "comStmt Break \n"
  | Continue loc -> prints "comStmt Continue \n"
  | If (exp, tb, fb, loc ) -> prints "comStmt If Start \n";
			      comExpr exp;
			      let btypestr = !recString in
			      recString :="";
			      comBlock tb;
			      let flag1 = isContainMF !recString in
			      let tbrec = !recString in
			      recString := ""; 
			      comBlock fb;
			      let flag2 = isContainMF !recString in
			      let fbrec = !recString in
			      recString := btypestr ;
			      begin
				if (flag1 || flag2)
				then
				  begin
			       	    match flag1, flag2 with
				    | (true, true) ->
			     concatChars (comLastCharacter !recString ) ("(" ^ tbrec ^","^fbrec ^")") 
				    | (true, false) ->
			     concatChars (comLastCharacter !recString ) ("(" ^ tbrec ^","^ "0" ^")")
				    | (false, true) ->
			     concatChars (comLastCharacter !recString ) ("(" ^ "0" ^","^ fbrec ^")")
				    | (false , false )-> ()
				  end
			      end; 
			      prints "comStmt If End\n"
  | Switch (exp, blk, stmlist, loc) -> prints "comStmt Switch Start \n";
				       comExpr exp;
				       comBlock blk;
				       (* comStmts stmlist; *)
				       prints "comStmt Switch End \n"
  | Loop (blk, loc, stmopt, stm2opt) -> prints "comStmt Loop Start \n";
					comBlock blk;
					 prints "comStmt Loop  End\n"
  | Block blk -> prints "comStmt block start \n";
		 comBlock blk;
		 prints "comStmt block end \n"
  | TryFinally (blk1, blk2, loc) -> prints "comStmt TryFinally  Start\n";
				    comBlock blk1;
				    comBlock blk2;
				    prints "comStmt TryFinally End \n"
  | TryExcept (blk1, (inslist, exp), blk2, loc) -> prints "comStmt TryExcept Start \n";
						   comBlock blk1;
						   comExpr exp;
						   comBlock blk2;
						   prints "comStmt TryExcept End\n"

(* deal with instructions*)
and comInstrs ins : unit  = List.iter comInstr ins

(*deal with one instruction *)
and comInstr instr :unit  =
  match instr with
  | Set (lv, exp, loc ) ->  prints "comInstr set Start \n";
			    comLval lv;
			    comExpr exp;
			    prints "comInstr set End \n"
  | Call (Some lv, exp, explist, loc) -> prints "comInstr scall Start\n";
					 comLval lv; comExpr exp;
					 (prints " -list : " ; List.iter comExpr explist);
					 prints " -list \n";
					 prints "comInstr scall End \n"
  | Call (None, exp, explist, loc) -> prints "comInstr ncall Start\n";
				      comExpr exp;
				      List.iter comExpr explist;
				      prints "comInstr ncall End \n"
  | Asm _ -> prints " comInstr Asm Start \n";
	     prints "comInstr Asm End \n "

(*deal with Lval *)
and comLval (lv : lval)  =
  match lv with
    (lhost, offset) ->  prints "comLval Start \n" ; comLhost lhost; comOffset offset ;
			prints "comLval End \n"

(*deal with Lval's lhost *)			 
and  comLhost (lhost : lhost) =
  match lhost with 
  | Var varinfo -> prints (" comLhost Var Start:  " ^ varinfo.vname ^ " \n");
		   concatChars (comLastCharacter !recString) varinfo.vname;
		   prints " comLhost Var End \n"
  | Mem exp -> prints "comLhost Mem Start\n";
	       comExpr exp;
	       prints "comLhost Mem End\n"

(*deal with Lval's offset *)
and comOffset (offset : offset) =
  match offset with
  | NoOffset ->  prints "comOffset nooffset \n"
  | Field (filedinfo, offset) ->  prints "comOffset Field Start \n";
				  comOffset offset;
				  prints "comOffset Field End \n"
  | Index (exp, offset) ->  prints "comOffset Index Start \n";
			    comExpr exp;
			    comOffset offset ;
			    prints "comOffset Index End\n" 

(* get the name of lval *)
(* and  comValofLval (lv: lval) = *)
(*   match lv with *)
(*   | (Var varinfo, offset) ->  varinfo.vname *)
(*   | _ -> "" *)

(*deal with expresion *)
and comExpr (expr : exp) =
  match expr with
  | Const v -> prints "comExpr Const v\n"
  | Lval lval -> prints "comExpr Lval Start\n";
		 comLval lval ;
		 prints " comExpr Lval End\n"
  | SizeOf typ -> prints "comExpr sizeof v\n"
  | SizeOfE exp -> prints "comExpr sizeofE Start \n";
		   comExpr exp;
		   prints "comExpr sizeofE End \n";
  | SizeOfStr strs -> prints "comExpr sizeofstr \n"
  | AlignOf typ -> prints "comExpr Alignof \n"
  | AlignOfE exp -> prints "comExpr AlignofE Start \n";
		    comExpr exp;
		    prints "comExpr AlignofE End \n"
  | UnOp (op, exp, typ) -> prints "comExpr UnOp Start  \n";
			   comExpr exp;
			   prints "comExpr UnOp End \n"
  | BinOp (bop, exp1, exp2, typ) -> prints "comExpr Binop Start\n";
				    comExpr exp1;
				    comExpr exp2;
				    prints "comExpr Binop End\n"
  | Question (exp1, exp2, exp3, typ) -> prints "comExpr Question Start\n";
					comExpr exp1;
					comExpr exp2;
					comExpr exp3;
					prints "comExpr Question End\n"
  | CastE (typ, exp) -> prints "comExpr  CastE Start\n";
			comExpr exp;
			prints "comExpr  CastE End \n"
  | AddrOf lval -> prints "comExpr Addrof Start\n";
		   comLval lval ;
		   prints "comExpr Addrof End \n"
  | AddrOfLabel ref_stmt -> prints "comExpr AddrofLabel\n"
  | StartOf lval -> prints "comExpr StartOf Start\n";
		    comLval lval;
		    prints "comExpr StartOf End\n"
		    

(* and comExprName ( expr: exp) = *)
(*   match expr with *)
(*   | Lval (Var v, offset) ->  *)
(*   | Lval (Mem exp, offset) ->  *)
(*   | _ ->  *)

(* get the last character *)
and comLastCharacter (strs : string) : string =
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
							  
  
(* main function *)	   
let _ = Abs.main ();
	print_string "\n-----------------------------------------------\n";
	printFunlist !funlist;
	print_string "-----------------------------------------------\n";
	
