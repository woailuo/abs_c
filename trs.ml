open Cil
open Utype
open Uexception
module ES = ExtString.String
module E = Errormsg

let funslist = Abs.funlist

let bhtString = ref ""
let funcName = ref ""

let rec  trsBeh funlist = List.iter trsFun !funlist

and  trsFun frecord =
  match frecord with
  | (fname, record) when fname <> "" -> bhtString := "";
					funcName := fname;
					trsFunBody record.funbody;
					record.bType <- !bhtString
  | _ -> ()

and  trsFunBody fbody =  trsBlock fbody.sbody

and trsBlock fblock = List.iter trsStmt fblock.bstmts

and trsStmt (stm: stmt)  = 
  match stm.skind with
  | Instr ilist -> prints "trsStmt Instr Start \n" ;
		   trsInstrs  ilist;
		   prints "trsStmt Instr End\n"
  | Return (Some exp, loc) -> prints "trsStmt Return Start\n";
  | Return (refStmt, loc) -> prints "trsStmt Return \n"; print_string "test \n" ;
  | Goto ( _ ,loc) -> prints "trsStmt Goto \n"
  | ComputedGoto (exp, loc) -> prints "trsStmt trsputeGoto Start \n";
  | Break loc -> prints "trsStmt Break \n"
  | Continue loc -> prints "trsStmt Continue \n"
  | If (exp, tb, fb, loc ) -> prints "trsStmt If Start \n";
  | Switch (exp, blk, stmlist, loc) -> prints "trsStmt Switch Start \n";
				       prints "trsStmt Switch End \n"
  | Loop (blk, loc, stmopt, stm2opt) -> prints "trsStmt Loop Start \n";
					 prints "trsStmt Loop  End\n"
  | Block blk -> prints "trsStmt block start \n";
		 prints "trsStmt block end \n"
  | TryFinally (blk1, blk2, loc) -> prints "trsStmt TryFinally  Start\n";
				    prints "trsStmt TryFinally End \n"
  | TryExcept (blk1, (inslist, exp), blk2, loc) -> prints "trsStmt TryExcept Start \n";
						   prints "trsStmt TryExcept End\n"

(* deal with instructions*)
and trsInstrs ins : unit  = List.iter trsInstr ins

(*deal with one instruction *)
and trsInstr instr :unit  =
  match instr with
  | Set (lv, exp, loc ) ->  prints "trsInstr set Start \n";
			    prints "trsInstr set End \n"
  | Call (Some lv, exp, explist, loc) -> prints "trsInstr scall Start\n";
					 prints "trsInstr scall End \n"
  | Call (None, exp, explist, loc) -> prints "trsInstr ncall Start\n";
				      prints "trsInstr ncall End \n"
  | Asm _ -> prints " trsInstr Asm Start \n";
	     prints "trsInstr Asm End \n "

(*deal with Lval *)
and trsLval (lv : lval)  =
  match lv with
    (lhost, offset) ->  prints "trsLval Start \n" ; trsLhost lhost; trsOffset offset ;
			prints "trsLval End \n"

(*deal with Lval's lhost *)			 
and  trsLhost (lhost : lhost) =
  match lhost with 
  | Var varinfo -> prints (" trsLhost Var Start:  " ^ varinfo.vname ^ " \n");
  | Mem exp -> prints "trsLhost Mem Start\n";
	       prints "trsLhost Mem End\n"

(*deal with Lval's offset *)
and trsOffset (offset : offset) =
  match offset with
  | NoOffset ->  prints "trsOffset nooffset \n"
  | Field (filedinfo, offset) ->  prints "trsOffset Field Start \n";
				  prints "trsOffset Field End \n"
  | Index (exp, offset) ->  prints "trsOffset Index Start \n";
			    prints "trsOffset Index End\n" 

(*deal with expresion *)
and trsExpr (expr : exp) =
  match expr with
  | Const v -> prints "trsExpr Const v\n"
  | Lval lval -> prints "trsExpr Lval Start\n";
		 prints " trsExpr Lval End\n"
  | SizeOf typ -> prints "trsExpr sizeof v\n"
  | SizeOfE exp -> prints "trsExpr sizeofE Start \n";
		   prints "trsExpr sizeofE End \n";
  | SizeOfStr strs -> prints "trsExpr sizeofstr \n"
  | AlignOf typ -> prints "trsExpr Alignof \n"
  | AlignOfE exp -> prints "trsExpr AlignofE Start \n";
		    prints "trsExpr AlignofE End \n"
  | UnOp (op, exp, typ) -> prints "trsExpr UnOp Start  \n";
			   prints "trsExpr UnOp End \n"
  | BinOp (bop, exp1, exp2, typ) -> prints "trsExpr Binop Start\n";
				    prints "trsExpr Binop End\n"
  | Question (exp1, exp2, exp3, typ) -> prints "trsExpr Question Start\n";
					prints "trsExpr Question End\n"
  | CastE (typ, exp) -> prints "trsExpr  CastE Start\n";
			prints "trsExpr  CastE End \n"
  | AddrOf lval -> prints "trsExpr Addrof Start\n";
		   prints "trsExpr Addrof End \n"
  | AddrOfLabel ref_stmt -> prints "trsExpr AddrofLabel\n"
  | StartOf lval -> prints "trsExpr StartOf Start\n";
		    prints "trsExpr StartOf End\n"

(* get the last character *)
and trsLastCharacter (strs : string) : string =
  let len  = String.length strs in
  if (len > 0) then
    (String.make 1 strs.[len-1])
  else 
    !bhtString
     
and  concatChars str  varinfo =
  if (varinfo = "malloc" || varinfo = "free" || varinfo = "%return%" || (String.contains varinfo '(') ) then
    begin
      match str with
      | ")" -> bhtString := !bhtString ^ ";" ^ varinfo 
      | "(" ->  bhtString := !bhtString ^ varinfo 
      | ";" ->  bhtString := !bhtString ^ varinfo
      | "c"| "e" -> bhtString := !bhtString ^ ";" ^ varinfo
      | "" -> bhtString :=  varinfo  
      | _ ->  bhtString :=  !bhtString ^";" ^varinfo 
    end
  else
    begin
      let record =
	try List.assoc ("%"^varinfo^"%") !funslist with
	  _ -> {fName = ""; bType = ""; funbody = Main.funbody  } 
      in
      if(record.fName <> "") then (* if equal , then () *)
	begin
	  match str with
	    ")" -> bhtString := !bhtString ^ ";" ^ record.fName
	  | "(" ->  bhtString := !bhtString ^ record.fName
	  | ";" ->  bhtString := !bhtString ^ record.fName
	  | "c"| "e" -> bhtString := !bhtString ^ ";" ^ record.fName
	  | "" -> bhtString :=  "%"^ varinfo ^ "%"  
	  | _ ->  bhtString :=  !bhtString ^";" ^ "%" ^ varinfo ^"%" 
	end
      else
	()
    end
and isLastStmt stmt =
  match stmt.succs with
  | [] -> true
  | hd :: tl -> false
		  
and isContainMF str =
  let bm = String.contains str 'm'  in
  let bf = String.contains str 'f' in
  bm || bf
  
(* main function *)	   
let main () = Abs.main (); trsBeh funslist
	     
