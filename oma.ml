open Cil
open Utype
open Uexception
module E = Errormsg

let funslist = Abs.funlist

let bhtString = ref ""
let funcName = ref ""
let flag = ref true 
let flag2 = ref true
		
let rec  comBeh funlist = List.iter comFun !funlist

and  comFun frecord =
  match frecord with
  | (fname, record) when fname <> "" -> bhtString := "";
					funcName := fname;
					comFunBody record.funbody;
					record.bType <- !bhtString
  | _ -> ()

and  comFunBody fbody =  comBlock fbody.sbody.bstmts

(* and comBlock fblock =  List.iter comStmt_last  fblock.bstmts *)

and checkHasLast stmts =
  match stmts with
    [] -> true
  | h :: tl ->
     begin
       let haslast = hasLastStmt h in
       match  haslast with
	 true -> checkHasLast tl
       | false -> false
       | _ -> false 
     end

and  comBlock stmts =
  match stmts with
    [] -> ()
  | h :: tl -> let haslast = hasLastStmt h in
	       begin
		 match haslast with
		   true ->
		   begin
		     match h.skind with
		     | Instr ilist -> print_string "comStmt Instr Start \n" ;
				      comInstrs  ilist; comBlock tl;
				      print_string "comStmt Instr End\n"
		     | Return (Some exp, loc) -> print_string "comStmt Return Start\n";
						 comExpr exp;
						 concatChars (comLastCharacter !bhtString) "%return%" ; comBlock tl;
						 print_string "comStmt Return End\n"
		     | Return (refStmt, loc) -> print_string "comStmt Return \n"; print_string "test \n" ;  comBlock tl;
						print_string "this is a test \n";
		     | Goto ( _ ,loc) -> print_string "comStmt Goto \n"; comBlock tl;
		     | ComputedGoto (exp, loc) -> print_string "comStmt computeGoto Start \n";
						  comExpr exp; comBlock tl;
						  print_string "comStmt computeGoto End \n";
		     | Break loc -> print_string "comStmt Break \n"; comBlock tl;
		     | Continue loc -> print_string "comStmt Continue \n"; comBlock tl;
		     | If(Lval (Var varinfo, offset), tb, fb, loc) when Str.string_match (Str.regexp "lconst_") varinfo.vname 0 ->
			let r = Str.regexp "lconst_" in
			let constStr = Str.global_replace r "" varinfo.vname in
			let r2 = Str.regexp "\\$" in
			let constStr2 = Str.global_replace r2 "*" constStr in
			let constStr3 = "const" ^ "(" ^ constStr2 ^ ")" in
			let prebht = !bhtString in
			bhtString := "";
			comBlock tb.bstmts;
			let tbtype = !bhtString in
			let flag =   isContainMF tbtype  in
			begin
			  match flag with
			  | false -> bhtString := prebht
			  | true -> bhtString := prebht; concatChars (comLastCharacter !bhtString ) ( constStr3 ^ "(" ^ tbtype ^ ")") 
			end;
			comBlock tl;
		     | If (exp, tb, fb, loc ) ->
			print_string "comStmt If Start \n";
			comExpr exp; 
			let isp = isPointer exp in
			let prestr= if (isp) then "("^ (getStructure exp)^ ")" else "($)" in
			let btypestr = !bhtString in
			bhtString :="";
			let tb_flag_hasLast = checkHasLast tb.bstmts in
			comBlock tb.bstmts;
			let flag1 = isContainMF !bhtString in
			let tbrec = !bhtString in
			bhtString := "";
			let fb_flag_hasLast = checkHasLast fb.bstmts in
			comBlock fb.bstmts;
			let flag2 = isContainMF !bhtString in
			let fbrec = !bhtString in
			bhtString := btypestr ;
			if (tb_flag_hasLast && fb_flag_hasLast) then
			  begin
			    if (flag1 || flag2)   then
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
			  end
			else
			  begin
			    match tb_flag_hasLast, fb_flag_hasLast with
      			    | true, false ->

			       begin
      				 match flag1, flag2 with
      				 | (true, true) ->
				     begin
				      bhtString := "";
				      comBlock tl;
				      let lastbh = !bhtString in
      				      bhtString := btypestr;
      	  			      concatChars (comLastCharacter !bhtString ) (tbrec) ;
				      concatChars (comLastCharacter !bhtString ) (lastbh) ;
				      let left = !bhtString in 
      				      bhtString := btypestr;
      				      concatChars (comLastCharacter !bhtString ) (fbrec) ;
      				      let right = !bhtString in
      				      bhtString := "(" ^ left ^ " + " ^ right ^ ")";
      				    end
				 | (true, false) ->
      				    begin
				      bhtString := "";
				      comBlock tl;
				      let lastbh = !bhtString in
      				      bhtString := btypestr;
      	  			      concatChars (comLastCharacter !bhtString ) (tbrec) ;
				      concatChars (comLastCharacter !bhtString ) (lastbh) ;
      				      let left = !bhtString in 
      				      bhtString := btypestr;
      				      let right = !bhtString in
      				      bhtString := "(" ^ left ^ " + " ^ right ^ ")";
      				    end
				 | (false, true) ->
				    begin
				      bhtString := "";
				      comBlock tl;
				      let lastbh = !bhtString in
      				      bhtString := btypestr;
				      concatChars (comLastCharacter !bhtString ) (lastbh) ;
      				      let left = !bhtString in
				      bhtString := btypestr;
				      concatChars (comLastCharacter !bhtString ) (fbrec) ;
      				      let right = !bhtString in
      				      bhtString := "(" ^ left ^ " + " ^ right ^ ")";
				    end
				 | (false, false) ->
				    begin
				      bhtString := "";
				      comBlock tl;
				      let lastbh = !bhtString in
      				      bhtString := btypestr;
				      concatChars (comLastCharacter !bhtString ) (lastbh) ;
      				      let left = !bhtString in 
      				      bhtString := btypestr;
      				      let right = !bhtString in
      				      bhtString := "(" ^ left ^ " + " ^ right ^ ")";
				    end
			       end

			       
      			    | false, true ->

			       begin
      				 match flag1, flag2 with
      				 | (true, true) ->
				     begin
				      bhtString := "";
				      comBlock tl;
				      let lastbh = !bhtString in
      				      bhtString := btypestr;
      	  			      concatChars (comLastCharacter !bhtString ) (tbrec) ;
				      let left = !bhtString in 
      				      bhtString := btypestr;
      				      concatChars (comLastCharacter !bhtString ) (fbrec) ;
				      concatChars (comLastCharacter !bhtString ) (lastbh) ;
      				      let right = !bhtString in
      				      bhtString := "(" ^ left ^ " + " ^ right ^ ")";
      				    end
				 | (true, false) ->
      				    begin
				      bhtString := "";
				      comBlock tl;
				      let lastbh = !bhtString in
      				      bhtString := btypestr;
      	  			      concatChars (comLastCharacter !bhtString ) (tbrec) ;
      				      let left = !bhtString in 
      				      bhtString := btypestr;
      				      concatChars (comLastCharacter !bhtString ) (lastbh) ;
      				      let right = !bhtString in
      				      bhtString := "(" ^ left ^ " + " ^ right ^ ")";
      				    end
				 | (false, true) ->
				    begin
				      bhtString := "";
				      comBlock tl;
				      let lastbh = !bhtString in
      				      bhtString := btypestr;
      				      let left = !bhtString in 
      				      bhtString := btypestr;
				      concatChars (comLastCharacter !bhtString ) (fbrec) ;
      				      concatChars (comLastCharacter !bhtString ) (lastbh) ;
      				      let right = !bhtString in
      				      bhtString := "(" ^ left ^ " + " ^ right ^ ")";
				    end
				 | (false, false) ->
				    begin
				      bhtString := "";
				      comBlock tl;
				      let lastbh = !bhtString in
      				      bhtString := btypestr;
      				      let left = !bhtString in 
      				      bhtString := btypestr;
      				      concatChars (comLastCharacter !bhtString ) (lastbh) ;
      				      let right = !bhtString in
      				      bhtString := "(" ^ left ^ " + " ^ right ^ ")";
				    end
			       end
				 
      			    | (false , false )->

			       begin
      				 match flag1, flag2 with
      				 | (true, true) ->
				     begin
				      bhtString := "";
				      comBlock tl;
				      let lastbh = !bhtString in
      				      bhtString := btypestr;
      	  			      concatChars (comLastCharacter !bhtString ) (tbrec) ;
				      let left = !bhtString in 
      				      bhtString := btypestr;
      				      concatChars (comLastCharacter !bhtString ) (fbrec) ;
      				      let right = !bhtString in
      				      bhtString := "(" ^ left ^ " + " ^ right ^ ")";
      				    end
				 | (true, false) ->
      				    begin
				      bhtString := "";
				      comBlock tl;
				      let lastbh = !bhtString in
      				      bhtString := btypestr;
      	  			      concatChars (comLastCharacter !bhtString ) (tbrec) ;
      				      let left = !bhtString in 
      				      bhtString := btypestr;
      				      let right = !bhtString in
      				      bhtString := "(" ^ left ^ " + " ^ right ^ ")";
      				    end
				 | (false, true) ->
				    begin
				      bhtString := "";
				      comBlock tl;
				      let lastbh = !bhtString in
      				      bhtString := btypestr;
      				      let left = !bhtString in
				      bhtString := btypestr;
				      concatChars (comLastCharacter !bhtString ) (fbrec) ;
      				      let right = !bhtString in
      				      bhtString := "(" ^ left ^ " + " ^ right ^ ")";
				    end
				 | (false, false) ->
				    begin
				      (* bhtString := ""; *)
				      (* comBlock tl; *)
				      (* let lastbh = !bhtString in *)
      				      (* bhtString := btypestr; *)
      				      (* let left = !bhtString in  *)
      				      (* bhtString := btypestr; *)
      				      (* let right = !bhtString in *)
      				      (* bhtString := "(" ^ left ^ " + " ^ right ^ ")"; *)
				    end
			       end
						   
						   
      			  end ;
			print_string "comStmt If End \n";
			
		     | Switch (exp, blk, stmlist, loc) -> print_string "comStmt Switch Start \n";  comBlock tl;
							  (* comExpr exp; *)
							  (* comBlock blk; *)
							  (* comStmts stmlist; *)
							  print_string "comStmt Switch End \n"
		     | Loop (blk, loc, stmopt, stm2opt) -> print_string "comStmt Loop Start \n"; comBlock tl;
							   (* comBlock blk; *)
							   print_string "comStmt Loop  End\n"
		     | Block blk -> print_string "comStmt block start \n";
				    (* comBlock blk; *) comBlock tl;
				    print_string "comStmt block end \n"
		     | TryFinally (blk1, blk2, loc) -> print_string "comStmt TryFinally  Start\n";
						       (* comBlock  blk1; *)
						   (* comBlock   blk2; *) comBlock tl;
						   print_string "comStmt TryFinally End \n"
		 | TryExcept (blk1, (inslist, exp), blk2, loc) -> print_string "comStmt TryExcept Start \n";
								  (* comBlock  blk1; *)
								  (* comExpr exp; *)
								  (* comBlock  blk2; *) comBlock tl;
								  print_string "comStmt TryExcept End\n"
	       end
  | false -> ()
  | _ -> ()
end

 (* and comStmList stmts = List.iter comStmt_last   stmts *)
				 
(* and comBlock_complic fblock =  List.iter comStmt   fblock.bstmts				  *)

(* and comStmt_test (stm: stmt)  = *)
(*   let haslast = hasLastStmt stm in *)
(*   match stm.skind with *)
(*   | Instr ilist -> print_string "comStmt Instr Start \n" ; *)
(* 		   	   comInstrs  ilist; *)
(* 		   print_string "comStmt Instr End\n" *)
(*   | Return (Some exp, loc) -> print_string "comStmt Return Start\n"; *)
(* 			      print_string "comStmt Return End\n" *)
(*   | Return (refStmt, loc) -> print_string "comStmt Return \n"; print_string "test \n" ; *)
(*     print_string "this is a test \n"; *)
(*   | Goto ( _ ,loc) -> print_string "comStmt Goto \n" *)
(*   | ComputedGoto (exp, loc) -> print_string "comStmt computeGoto Start \n"; *)
(* 			       print_string "comStmt computeGoto End \n"; *)
(*   | Break loc -> print_string "comStmt Break \n" *)
(*   | Continue loc -> print_string "comStmt Continue \n" *)
(*   | If (exp, tb, fb, loc ) -> print_string "comStmt If Start \n"; *)
(* 			      print_string " if stmt succes start  \n"; *)
(* 			      (if haslast then comStmList stm.succs else (print_string  " shi bai\n")); *)
			      
(* 			      print_string " if stmt succes end  \n"; *)
(* 			      print_string "comStmt If End\n" *)
(*   | Switch (exp, blk, stmlist, loc) -> print_string "comStmt Switch Start \n"; *)
(* 				       print_string "comStmt Switch End \n" *)
(*   | Loop (blk, loc, stmopt, stm2opt) -> print_string "comStmt Loop Start \n"; *)
(* 					 print_string "comStmt Loop  End\n" *)
(*   | Block blk -> print_string "comStmt block start \n"; *)
(* 		 print_string "comStmt block end \n" *)
(*   | TryFinally (blk1, blk2, loc) -> print_string "comStmt TryFinally  Start\n"; *)

(* 				    print_string "comStmt TryFinally End \n" *)
(*   | TryExcept (blk1, (inslist, exp), blk2, loc) -> print_string "comStmt TryExcept Start \n"; *)
(* 						   print_string "comStmt TryExcept End\n" *)
					 
(* and comStmt_last (stm: stmt)  = *)
(*   let haslast = hasLastStmt stm in *)
(*   begin *)
(*     match haslast  with *)
(*     | true when !flag ->   comStmt stm  *)
(*     | false ->   flag := false *)
(*     | _ ->  flag := false *)
(*   end *)

(* and comStmt (stm: stmt)  = *)
(*   (\* let haslast = hasLastStmt stm in *\) *)
(*   (\* if (haslast && !flag) then begin *\) *)
(*   match stm.skind with *)
(*   | Instr ilist -> print_string "comStmt Instr Start \n" ; *)
(* 		   comInstrs  ilist; *)
(* 		   print_string "comStmt Instr End\n" *)
(*   | Return (Some exp, loc) -> print_string "comStmt Return Start\n"; *)
(* 			      comExpr exp; *)
(* 			      concatChars (comLastCharacter !bhtString) "%return%" ; *)
(* 			      print_string "comStmt Return End\n" *)
(*   | Return (refStmt, loc) -> print_string "comStmt Return \n"; print_string "test \n" ; *)
(*     print_string "this is a test \n"; *)
(*   | Goto ( _ ,loc) -> print_string "comStmt Goto \n" *)
(*   | ComputedGoto (exp, loc) -> print_string "comStmt computeGoto Start \n"; *)
(* 			       comExpr exp; *)
(* 			       print_string "comStmt computeGoto End \n"; *)
(*   | Break loc -> print_string "comStmt Break \n" *)
(*   | Continue loc -> print_string "comStmt Continue \n" *)
(*   | If(Lval (Var varinfo, offset), tb, fb, loc) when Str.string_match (Str.regexp "lconst_") varinfo.vname 0 -> *)
(*     let r = Str.regexp "lconst_" in *)
(*     let constStr = Str.global_replace r "" varinfo.vname in *)
(*     let r2 = Str.regexp "\\$" in *)
(*     let constStr2 = Str.global_replace r2 "*" constStr in *)
(*     let constStr3 = "const" ^ "(" ^ constStr2 ^ ")" in *)
(*     let prebht = !bhtString in *)
(*     bhtString := ""; *)
(*     comBlock tb.bstmts; *)
(*     let tbtype = !bhtString in *)
(*     let flag =   isContainMF tbtype  in *)
(*     begin *)
(*       match flag with *)
(*         | false -> bhtString := prebht *)
(*         | true -> bhtString := prebht; concatChars (comLastCharacter !bhtString ) ( constStr3 ^ "(" ^ tbtype ^ ")")  *)
(*     end *)
(*   | If (exp, tb, fb, loc ) -> print_string "comStmt If Start \n"; *)
(*       comExpr exp; *)
(*       let isp = isPointer exp in *)
(*       let prestr= if (isp) then "("^ (getStructure exp)^ ")" else "($)" in *)
(*       let btypestr = !bhtString in *)
(*       bhtString :=""; *)

(*       flag := true; *)
(*       print_string " tb Start \n"; *)
(*       comBlock  tb.bstmts; *)
(*       print_string " tb End \n"; *)
(*       let tb_flag = !flag in *)
(*       (\* (if tb_flag then print_string  "ture \n" else print_string  "false\n"); *\) *)

(*       let flag1 = isContainMF !bhtString in *)
(*       let tbrec = !bhtString in *)
(*       bhtString := ""; *)

(*       flag := true; *)
(*       print_string " fb Start \n"; *)
(*       comBlock  fb.bstmts; *)
(*       print_string " fb End \n"; *)

(*       let fb_flag = !flag in *)
(*       (\* (if fb_flag then print_string  "ture \n" else print_string  "false\n"); *\) *)

(*       let flag2 = isContainMF !bhtString in *)
(*       let fbrec = !bhtString in *)
(*       bhtString := btypestr ; *)
      
(*       if(tb_flag && fb_flag) then *)
(*       	begin *)
(*       begin *)
(* 	if (flag1 || flag2) *)
(* 	then *)
(* 	  begin *)
(* 	    match flag1, flag2 with *)
(* 	      | (true, true) -> *)
(* 		concatChars (comLastCharacter !bhtString ) (prestr ^"(" ^ tbrec ^","^fbrec ^")")  *)
(* 	      | (true, false) -> *)
(* 		concatChars (comLastCharacter !bhtString ) (prestr ^"(" ^ tbrec ^","^ "0" ^")") *)
(* 	      | (false, true) -> *)
(* 		concatChars (comLastCharacter !bhtString ) (prestr ^ "(" ^ "0" ^","^ fbrec ^")") *)
(* 	      | (false , false )-> () *)
(* 	  end *)
(*       end; *)
(*       print_string "comStmt If End\n" *)
(*       	end *)
(*       else *)
(*       	begin *)
(*       	  match tb_flag, fb_flag with *)
(*       	  | true, false ->  () *)
(*       	  | false, true -> *)
(*       	     begin *)
(*       	       match flag1, flag2 with *)
(*       	       | (true, true) -> *)
(*       		  bhtString := "(" ^ !bhtString ^ tbrec ^ " + " ^ !bhtString ^ fbrec ^ ")" *)
(*       	       | (true, false) -> *)
(*       	      	  begin *)
(*       		      bhtString :=""; *)
(*       		      flag := true; *)
(*       		      (\* comStmList stm.succs; *\) *)
(* 		      flag := true; *)
(*       		      print_string  !bhtString; *)
(*       		      let lastbh = !bhtString in *)
(*       		      bhtString := btypestr ; *)
(*       	  	      concatChars (comLastCharacter !bhtString ) (tbrec) ; *)
(*       		      let left = !bhtString in  *)
(*       		      bhtString := btypestr; *)
(*       		      concatChars (comLastCharacter !bhtString ) (lastbh) ; *)
(*       		      let right = !bhtString in *)
(*       		      bhtString := "(" ^ left ^ " + " ^ right ^ ")"; *)
(*       		      print_string  "\n this is a test \n"; *)
(*       		    end *)
(*       	       | (false, true) -> begin *)
(*       		  (\* if haslast then *\) *)
(*       		  (\*   begin *\) *)
(*       		      bhtString :=""; *)
(*       		      comStmt stm; *)
(*       		      let lastbh = !bhtString in *)
(*       		      bhtString := btypestr ; *)
(*       		      bhtString := "(" ^ !bhtString ^  " + " ^ !bhtString ^ fbrec ^ lastbh ^ ")"; *)
(*       		    end *)
(*       		  (\* else *\) *)
(*       		  (\*   begin *\) *)
(*       		  (\*     bhtString := "(" ^ !bhtString ^  " + " ^ !bhtString ^ fbrec ^ ")" *\) *)
(*       		  (\*   end *\) *)
(*       	       | (false , false )-> *)
(*       		  bhtString :=  "mmmmmmmm" *)
(*       	     end *)
(*       	  | false, false -> print_string  "mmmmmmx \n" *)
(*       	end *)
(*   | Switch (exp, blk, stmlist, loc) -> print_string "comStmt Switch Start \n"; *)
(* 				       (\* comExpr exp; *\) *)
(* 				       (\* comBlock blk; *\) *)
(* 				       (\* comStmts stmlist; *\) *)
(* 				       print_string "comStmt Switch End \n" *)
(*   | Loop (blk, loc, stmopt, stm2opt) -> print_string "comStmt Loop Start \n"; *)
(* 					(\* comBlock blk; *\) *)
(* 					 print_string "comStmt Loop  End\n" *)
(*   | Block blk -> print_string "comStmt block start \n"; *)
(* 		 (\* comBlock blk; *\) *)
(* 		 print_string "comStmt block end \n" *)
(*   | TryFinally (blk1, blk2, loc) -> print_string "comStmt TryFinally  Start\n"; *)
(* 				    (\* comBlock  blk1; *\) *)
(* 				    (\* comBlock   blk2; *\) *)
(* 				    print_string "comStmt TryFinally End \n" *)
(*   | TryExcept (blk1, (inslist, exp), blk2, loc) -> print_string "comStmt TryExcept Start \n"; *)
(* 						   (\* comBlock  blk1; *\) *)
(* 						   (\* comExpr exp; *\) *)
(* 						   (\* comBlock  blk2; *\) *)
(* 						   print_string "comStmt TryExcept End\n" *)
    (* end *)
  (* else *)
  (*   begin *)
  (*     print_string  " this ia a test for no last \n"; *)
  (*   end *)


(* deal with instructions*)
and comInstrs ins : unit  = List.iter comInstr ins

(*deal with one instruction *)
and comInstr instr :unit  =
  match instr with
  | Set (lv, exp, loc ) ->  print_string "comInstr set Start \n";
			    comLval lv;
			    comExpr exp;
			    print_string "comInstr set End \n"
  | Call (Some lv, exp, explist, loc) -> print_string "comInstr scall Start\n";
					 comLval lv; comExpr exp;
					 (print_string " -list : " ; List.iter comExpr explist);
					 print_string " -list \n";
					 print_string "comInstr scall End \n"
  | Call (None, exp, explist, loc) -> print_string "comInstr ncall Start\n";
				      comExpr exp;
				      List.iter comExpr explist;
				      print_string "comInstr ncall End \n"
  | Asm _ -> print_string " comInstr Asm Start \n";
	     print_string "comInstr Asm End \n "

(*deal with Lval *)
and comLval (lv : lval)  =
  match lv with
    (lhost, offset) ->  print_string   "comLval Start \n" ; comLhost lhost; comOffset offset ;
			print_string   "comLval End \n"

(*deal with Lval's lhost *)
and  comLhost (lhost : lhost) =
  match lhost with
  | Var varinfo -> print_string   (" comLhost Var Start:  " ^ varinfo.vname ^ " \n");
		   concatChars (comLastCharacter !bhtString) varinfo.vname
  | Mem exp -> print_string   "comLhost Mem Start\n";
	       comExpr exp;
	       print_string   "comLhost Mem End\n"

(*deal with Lval's offset *)
and comOffset (offset : offset) =
  match offset with
  | NoOffset ->  print_string   "comOffset nooffset \n"
  | Field (filedinfo, offset) ->  print_string   "comOffset Field Start \n";
				  comOffset offset;
				  print_string   "comOffset Field End \n"
  | Index (exp, offset) ->  print_string   "comOffset Index Start \n";
			    comExpr exp;
			    comOffset offset ;
			    print_string   "comOffset Index End\n" 

(*deal with expresion *)
and comExpr (expr : exp) =
  match expr with
  | Const v -> print_string   "comExpr Const v\n"
  | Lval lval -> print_string   "comExpr Lval Start\n";
		 comLval lval ;
		 print_string   " comExpr Lval End\n"
  | SizeOf typ -> print_string   "comExpr sizeof v\n"
  | SizeOfE exp -> print_string   "comExpr sizeofE Start \n";
		   comExpr exp;
		   print_string   "comExpr sizeofE End \n";
  | SizeOfStr strs -> print_string   "comExpr sizeofstr \n"
  | AlignOf typ -> print_string   "comExpr Alignof \n"
  | AlignOfE exp -> print_string   "comExpr AlignofE Start \n";
		    comExpr exp;
		    print_string   "comExpr AlignofE End \n"
  | UnOp (op, exp, typ) -> print_string   "comExpr UnOp Start  \n";
			   comExpr exp;
			   print_string   "comExpr UnOp End \n"
  | BinOp (bop, exp1, exp2, typ) -> print_string   "comExpr Binop Start\n";
				    comExpr exp1;
				    comExpr exp2;
				    print_string   "comExpr Binop End\n"
  | Question (exp1, exp2, exp3, typ) -> print_string   "comExpr Question Start\n";
					comExpr exp1;
					comExpr exp2;
					comExpr exp3;
					print_string   "comExpr Question End\n"
  | CastE (typ, exp) -> print_string   "comExpr  CastE Start\n";
			comExpr exp;
			print_string   "comExpr  CastE End \n"
  | AddrOf lval -> print_string   "comExpr Addrof Start\n";
		   comLval lval ;
		   print_string   "comExpr Addrof End \n"
  | AddrOfLabel ref_stmt -> print_string   "comExpr AddrofLabel\n"
  | StartOf lval -> print_string   "comExpr StartOf Start\n";
		    comLval lval;
		    print_string   "comExpr StartOf End\n"

(* get the last character *)
and comLastCharacter (strs : string) : string =
  let len  = String.length strs in
  if (len > 0) then
    (String.make 1 strs.[len-1])
  else
    !bhtString

and  concatChars str  varinfo =
  if (varinfo = "malloc" || varinfo = "free" || varinfo = "%return%" || (String.contains varinfo '(')  || (String.contains varinfo ';')  ) then
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
      
and hasLastStmt stmt: bool  =
  match stmt.succs with
  | [] -> print_string "no last stat \n"; false
  | hd :: tl -> print_string " has last stat \n"; true
  | _ -> print_string " other stmt \n"; false


and isContainMF str =
  let bm = String.contains str 'm'  in
  let bf = String.contains str 'f' in
  bm || bf

and isPointer expr =
    match expr with
  Const    _ -> print_string   "const \n"; false
    | Lval   lv  -> print_string   "lval \n" ; isPointer2 lv
    | SizeOf   _ -> print_string   "sizeof \n" ; false
    | SizeOfE    _ -> print_string   "sizeofe \n"; false
    | SizeOfStr  _ -> print_string   "sizeofstr \n"; false
    | AlignOf      _ -> print_string   "alignof \n"; false
    | AlignOfE     _ -> print_string   "alignofe \n"; false
    | UnOp         _ -> print_string   "unop \n"; false
    | BinOp        _ -> print_string   "binop \n"; false
    | Question   _ -> print_string   "question \n"; false
    | CastE        _ -> print_string   "caste \n"; false
    | AddrOf     _ -> print_string   "addrof \n"; false
    | AddrOfLabel   _ -> print_string   "addroflabel \n"; false
    | StartOf     _ -> print_string   "startof \n"; false

and isPointer2  (lv : lval) =
  match lv with
    | (Var var, offset) ->  pointerType var.vtype
    | (Mem exp, offset) ->  isPointer exp

and pointerType (vtype:typ) : bool =
  match vtype with
    TVoid _   -> print_string   " tvoid\n"; false
  | TInt _  -> print_string   " tint\n";  false
  | TFloat  _ -> print_string   " tfloat\n";  false
  | TPtr (ptype,attrib) -> print_string   "pointer type \n"; true
  | TArray (typ, eop, attr) -> pointerType typ
  | TFun _ -> print_string   " tfun\n";  false
  | TNamed _  -> print_string   " tnamed\n";  false
  | TComp _ -> print_string   " tcomp\n";  false
  | TEnum _ -> print_string   " tenum\n";  false
  | TBuiltin_va_list  _ -> print_string   " tbuiltin_va_list\n";  false

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
  | Lval (Mem lve, Index _) ->print_string   " getstructure Index  \n"; ""
  | CastE (typ, exp)-> print_string   " rasise cast  \n";
                        getStructure exp
   | BinOp  (binop, e1, e2,typ) ->  print_string   " Start from binop to test structure \n";
                                    (getStructure e1) ^ (getStructure e2)
   | Const c  ->  (match c with
                     CInt64 (a,b,c) ->  print_string   " cint 64\n";   (string_of_int (i64_to_int a))
                   | CStr s -> print_string   (" cstr s : " ^ s ^ " \n");""
                   | CWStr _ -> print_string   " cwstr \n";""
                   | CChr _ -> print_string   " cchr  \n";""
                   | CReal _ -> print_string   " creal \n";""
                   | CEnum _ -> print_string   " cenum \n";"" )
    | _ -> print_string   " other is empty string: \n "; ""

and getOffset (offset : offset):string =
  match offset with
      NoOffset -> print_string   " get offset : no offset \n";  ""
    | Field(finfo, NoOffset) -> finfo.fname
    | Field(finfo, foffset) -> finfo.fname ^ "->"^getOffset foffset
    | Index(e, NoOffset) -> "[" ^ (getStructure e)^ "]"
    | Index(e, inoffset) -> "[" ^ (getStructure e)^ "]"  ^ (getOffset inoffset)

(* main function *)
let main () = Abs.main (); comBeh funslist

