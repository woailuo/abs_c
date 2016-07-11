open Cil
open Utype
open Uexception
module E = Errormsg

let funslist = Abs.funlist

let bhtString = ref ""
let funcName = ref ""

let rec  comBeh funlist = List.iter comFun !funlist

and  comFun frecord =
  match frecord with
  | (fname, record) when fname <> "" -> bhtString := "";
					funcName := fname;
					comFunBody record.funbody;
					record.bType <- !bhtString
  | _ -> ()

and  comFunBody fbody =  comBlock fbody.sbody

and comBlock fblock = List.iter comStmt fblock.bstmts

and comStmt (stm: stmt)  =
  isLastStmt stm;
  match stm.skind with
  | Instr ilist -> prints "comStmt Instr Start \n" ;
		   comInstrs  ilist;
		   prints "comStmt Instr End\n"
  | Return (Some exp, loc) -> prints "comStmt Return Start\n";
			      comExpr exp;
			      (* ( if(isLastStmt stm) then *)
			      (* concatChars (comLastCharacter !bhtString) "" *)
			      concatChars (comLastCharacter !bhtString) "%return%" ;
			      prints "comStmt Return End\n"
  | Return (refStmt, loc) -> prints "comStmt Return \n"; prints "test \n" ;
    prints "this is a test \n";
  | Goto ( _ ,loc) -> prints "comStmt Goto \n"
  | ComputedGoto (exp, loc) -> prints "comStmt computeGoto Start \n";
			       comExpr exp;
			       prints "comStmt computeGoto End \n";
  | Break loc -> prints "comStmt Break \n"
  | Continue loc -> prints "comStmt Continue \n"
  | If(Lval (Var varinfo, offset), tb, fb, loc) when Str.string_match (Str.regexp "lconst_") varinfo.vname 0 ->
    let r = Str.regexp "lconst_" in
    let constStr = Str.global_replace r "" varinfo.vname in
    let r2 = Str.regexp "\$" in
    let constStr2 = Str.global_replace r2 "*" constStr in
    let constStr3 = "const" ^ "(" ^ constStr2 ^ ")" in
    let prebht = !bhtString in
    bhtString := "";
    comBlock tb;
    let tbtype = !bhtString in
    let flag =   isContainMF tbtype  in
    begin
      match flag with
        | false -> bhtString := prebht
        | true -> bhtString := prebht; concatChars (comLastCharacter !bhtString ) ( constStr3 ^ "(" ^ tbtype ^ ")") 
    end
  | If (exp, tb, fb, loc ) -> prints "comStmt If Start \n";
      comExpr exp;
      let isp = isPointer exp in
      let prestr= if (isp) then "("^ (getStructure exp)^ ")" else "($)" in
      let btypestr = !bhtString in
      bhtString :="";
      comBlock tb;
      let flag1 = isContainMF !bhtString in
      let tbrec = !bhtString in
      bhtString := ""; 
      comBlock fb;
      let flag2 = isContainMF !bhtString in
      let fbrec = !bhtString in
      bhtString := btypestr ;
      begin
	if (flag1 || flag2)
	then
	  begin
	    match flag1, flag2 with
	      | (true, true) ->
		concatChars (comLastCharacter !bhtString ) (prestr ^"(" ^ tbrec ^","^fbrec ^")") 
	      | (true, false) ->
		concatChars (comLastCharacter !bhtString ) (prestr ^"(" ^ tbrec ^","^ "0" ^")")
	      | (false, true) ->
		concatChars (comLastCharacter !bhtString ) (prestr ^ "(" ^ "0" ^","^ fbrec ^")")
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
		   concatChars (comLastCharacter !bhtString) varinfo.vname
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

(* get the last character *)
and comLastCharacter (strs : string) : string =
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
  | [] -> prints "no last stat \n";true
  | hd :: tl -> prints " has last stat \n"; false

and isContainMF str =
  let bm = String.contains str 'm'  in
  let bf = String.contains str 'f' in
  bm || bf

and isPointer expr =
    match expr with
  Const    _ -> prints "const \n"; false
    | Lval   lv  -> prints "lval \n" ; isPointer2 lv
    | SizeOf   _ -> prints "sizeof \n" ; false
    | SizeOfE    _ -> prints "sizeofe \n"; false
    | SizeOfStr  _ -> prints "sizeofstr \n"; false
    | AlignOf      _ -> prints "alignof \n"; false
    | AlignOfE     _ -> prints "alignofe \n"; false
    | UnOp         _ -> prints "unop \n"; false
    | BinOp        _ -> prints "binop \n"; false
    | Question   _ -> prints "question \n"; false
    | CastE        _ -> prints "caste \n"; false
    | AddrOf     _ -> prints "addrof \n"; false
    | AddrOfLabel   _ -> prints "addroflabel \n"; false
    | StartOf     _ -> prints "startof \n"; false

and isPointer2  (lv : lval) =
  match lv with
    | (Var var, offset) ->  pointerType var.vtype
    | (Mem exp, offset) ->  isPointer exp

and pointerType (vtype:typ) : bool =
  match vtype with
    TVoid _   -> prints " tvoid\n"; false
  | TInt _  -> prints " tint\n";  false
  | TFloat  _ -> prints " tfloat\n";  false
  | TPtr (ptype,attrib) -> prints "pointer type \n"; true
  | TArray (typ, eop, attr) -> pointerType typ
  | TFun _ -> prints " tfun\n";  false
  | TNamed _  -> prints " tnamed\n";  false
  | TComp _ -> prints " tcomp\n";  false
  | TEnum _ -> prints " tenum\n";  false
  | TBuiltin_va_list  _ -> prints " tbuiltin_va_list\n";  false

and getStructure (expr : exp) : string =
  match expr with
  | Lval(Var vinfo, Index (e, offset)) ->
    (vinfo.vname) ^"[" ^(getStructure e)^"]"^ (getOffset offset)
  | Lval (Var vinfo, _) -> vinfo.vname
  | Lval (Mem lve, NoOffset) -> "*" ^ ( getStructure lve )
  | Lval (Mem lve, Field (ffinfo, NoOffset)) ->
     (getStructure lve ) ^ "->"^ ffinfo.fname
  | Lval (Mem lve, Field (ffinfo, foffset)) ->
     (getStructure lve ) ^ "->"^ ffinfo.fname ^ "->"^getOffset foffset
  | Lval (Mem lve, Index _) ->prints " getstructure Index  \n"; ""
  | CastE (typ, exp)-> prints " rasise cast  \n";
                        getStructure exp
   | BinOp  (binop, e1, e2,typ) ->  prints " Start from binop to test structure \n";
                                    (getStructure e1) ^ (getStructure e2)
   | Const c  ->  (match c with
                     CInt64 (a,b,c) ->  prints " cint 64\n";   (string_of_int (i64_to_int a))
                   | CStr s -> prints (" cstr s : " ^ s ^ " \n");""
                   | CWStr _ -> prints " cwstr \n";""
                   | CChr _ -> prints " cchr  \n";""
                   | CReal _ -> prints " creal \n";""
                   | CEnum _ -> prints " cenum \n";"" )
    | _ -> prints " other is empty string: \n "; ""

and getOffset (offset : offset):string =
  match offset with
      NoOffset -> prints " get offset : no offset \n";  ""
    | Field(finfo, NoOffset) -> finfo.fname
    | Field(finfo, foffset) -> finfo.fname ^ "->"^getOffset foffset
    | Index(e, NoOffset) -> "[" ^ (getStructure e)^ "]"
    | Index(e, inoffset) -> "[" ^ (getStructure e)^ "]"  ^ (getOffset inoffset)

(* main function *)
let main () = Abs.main (); comBeh funslist
