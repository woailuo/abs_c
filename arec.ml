open Cil
open Utype
open Uexception
module ES = ExtString.String
module E = Errormsg

let funcslist = Oma.funslist

(* print behavioral types of functions*)
let rec  printFunlist funlist =
  match funlist with
    [] -> ()
  | (fname, record) :: tl when fname <> "" ->
     print_string (record.fName ^ " : "); print_newline ();
     print_string  ( "   " ^ record.bType ); print_newline();
     printFunlist tl;
  | _ -> ()

and  rplFunclist funlist =
  List.iter rplReflexiveFun !funcslist; (*deal with self-recursive call function *)
  List.iter rplFun !funcslist (* deal with recursive call function *)

and rplFun funlist =
  match funlist with
  | (fname, record)  when ((fname <> "")&& (String.contains record.bType '%') )  ->
     rplFun2 record !funcslist
  | _ -> ()

and rplFun2 record funlist =
  match funlist with
  | [] -> ()
  | (newfname, newrecord ) :: tl  when newfname <> "" ->
     let r = Str.regexp newfname in
     let newBtype = 
       Str.global_replace  r newrecord.bType record.bType
     in
     record.bType <- newBtype;
     rplReflexiveFun (record.fName, record);
     rplFun2 record tl
  | _ -> ()

and  rplReflexiveFun func =
  match func with
  | (fname, record)  when (fname <> "" && String.contains record.bType '%') ->
     let (bflag, newBtype) = ES.replace record.bType fname "α" in
     if(bflag) then
       (
	 let betype =
	   Str.global_replace (Str.regexp fname) "α" newBtype
	 in
	 record.bType <- "(uα." ^ betype ^ ")"
       )
  | _ -> ()

and deleteSemi funlist =
  match funlist with
  | [] -> ()
  | (fname, record) :: tl ->
     let r = Str.regexp "\\([;]+\\)" in
     let betype =
       Str.global_replace r ";" record.bType
     in
     record.bType <- betype

and deleteBranch funlist =
  match funlist with
  | [] -> ()
  | (fname, record) :: tl ->
     let r = Str.regexp "\\((,)\\|(0,)\\|(,0)\\|(0,0)\\)" in
     let betype =
       Str.global_replace r "" record.bType
     in
     record.bType <- betype

and deleteFinalReturn funlist =
  match funlist with
  | [] -> ()
  | (fname, record) :: tl ->
     let r = Str.regexp "[.]*[;()]%return%$" in      (* "\\([.]*[;()]%return%\\)"     "[.]*[;()]%return%$"  *)
     let betype =
       Str.global_replace r "" record.bType
     in
     record.bType <- betype

let _ = Oma.main ();
	rplFunclist !funcslist;

        deleteBranch !funcslist;
        deleteFinalReturn !funcslist ;
        deleteSemi !funcslist;

	print_string "\n-----------------------------------------------\n";
	printFunlist !funcslist;
	print_string "-----------------------------------------------\n";
