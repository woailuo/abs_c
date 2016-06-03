open Cil
open Uexception
module E = Errormsg

let funclist = ref (("","") :: [])
let isRecuriveCall = ref false

let rec fixLval (fname:string) (lv: lval): string =
  match lv with
  | (Var a, _)  ->
     (
       try
         (List.assoc a.vname funclist.contents)
       with
         _ -> ""
     )
  | _ -> ""
n
and fixExpr (fname : string) (expr: exp) : string =
  match expr with
  | Lval a -> fixLval fname a
  | UnOp (unop, e1, typ) -> fixExpr fname e1
  | BinOp (binop, e1,e2, typ) -> let str1 = fixExpr  fname e1 in
                                 let str2 = fixExpr fname e2 in
                                 str1 ^ str2
  | Question (e1,e2,e3,typ) -> let str1 = fixExpr  fname e1 in
                               let str2 = fixExpr fname e2 in
                               let str3 = fixExpr fname e3 in
                                 str1 ^ str2 ^ str3
  | CastE (typ, e) -> let str1 = fixExpr fname e in str1
  | Const _
  | SizeOf _  | SizeOfE _ | SizeOfStr _  | AlignOf _
  | AlignOfE _
  | AddrOf _  | AddrOfLabel _  | StartOf _ -> ""

and fixinstrs (fname : string) (ins:instr list) : string =
  match ins with
  | i :: rest-> let s,b = fixInstr fname i in
                  if b then(s ^";" ^ (fixinstrs fname rest)) else (fixinstrs fname rest)
    | [] -> ""

and fixInstr (fname : string) (i : instr): string* bool  =
  match i with
  | Set _  -> "", false
  | Call (_, (Lval (Var a, NoOffset)), exprs ,location) when (a.vname = "free" || a.vname = "malloc")   ->  a.vname, true  (* todo : exprs *)
  | Call (_,((Lval (Var a, _)) as e1), exps, loc) ->
     let str1 = fixExpr fname e1 in
     if a.vname = fname then (isRecuriveCall:=true; ("a",true)) else
       (if (String.length str1) > 0 then (str1,true) else ("",false))
  | Call _ -> "",false
  | Asm _-> "",false

and findMorF (str: string): bool =
  let b =  (String.contains str 'm') || (String.contains str 'f') (* || (String.contains str 'a') *) in
  b

and isContainsSemi (str: string) : bool =
  let length = String.length str in
  if (length>0&& (findMorF str) )then
    (
      let substr = String.sub str (length-1) 1 in
      if substr = ")" then false else
        (
          if length < 7 then (String.contains str ';') else
            (
              let substr2 = String.sub str (length-7) 7 in
              if (String.contains substr2 ';') then true else false
            )
        )
    )
  else
    false

and deleteSemi (str: string) : string =
  let length = String.length str in
  if (length > 0) then
    (
      let substr = String.sub str (length-1) 1 in
      if substr = ";" then(deleteSemi (String.sub str 0 (length-1)))
      else str
    )
  else ""

and isCaseOrDefault (labels : label list) :bool =
       match labels with
         [] -> false
       | l :: rest -> (
         match l with
           Case _ -> true
         | Default _ ->  true
         | _ -> isCaseOrDefault rest
       )

and  isBreakStmt (s: stmt) : bool =
  match s.skind with
  Break _ -> true
  | _ -> false

and caseString = ref (("","") :: [])

and  findAllCases (fname :string) (casenum:int) (stmts : stmt list) : int =
  match stmts with
    [] -> casenum
  | s::rest -> let b = isCaseOrDefault s.labels in
               let isBreak = isBreakStmt s in   (*  todo:  consider case 1: .... case 2: ...;break , in case1 no break *)
               if(b) then (
                 let str = fixStmt fname s in
                 let strnum = (string_of_int (casenum + 1)) in
                 caseString := (strnum, str) :: !caseString;
                 let n=  findAllCases fname (casenum + 1) rest in
                 n
                              )
               else (
                 let str = fixStmt fname s in
                 let strnum = (string_of_int casenum) in
                 caseString := (strnum, str) :: !caseString;
                 let n = findAllCases fname casenum rest in
                 n
               );

and  mergecaselist = ref (("","") :: [])
and  caseZero = ref false

and mergeCases (num : int) (strlist: (string*string) list) : string=
  let length = List.length strlist in
  begin
    for i = 1 to num do
      let str1 = ref "" in
      let flagsemi = ref false in
    begin
      for j = 0 to length-1 do
        let (a,b) = (List.nth strlist j) in
        let boolean = (i = (int_of_string a)) in
        if boolean then ( str1 := ( if(findMorF b) then
                                      (
                                        if (findMorF (!str1)) then ( flagsemi := true ;(deleteSemi(!str1) ^ ";" ^ b)) else
                                          b
                                      )
                                    else (!str1)  ) )
      else ()
    done
    end ;
    (
        if (findMorF (!str1)) then
          (
            let newstring = !str1 in
            if !flagsemi  then ( mergecaselist := (string_of_int i, "(" ^ newstring ^ ")") :: !mergecaselist; ) else
              ( mergecaselist := (string_of_int i, newstring) :: !mergecaselist; )
          )
        else
          (
            caseZero := true;
          )
      ); flagsemi := false
  done
  end;
  (
    let len = List.length !mergecaselist in
    let newstr = ref "" in
    match len > 0 with
      false -> ""
    | true -> begin
              for i = 0 to len -1  do
                let (a,b) = List.nth !mergecaselist i in
                let b1 = isContainsSemi b in  (*   todo:  if malloc;free;(.. + ..) ?; not correct *)
                let b2 = findMorF (!newstr) in
                (
                  match b1,b2 with
                    true, true->  newstr :=  "(" ^ b ^ ")" ^ " + " ^ !newstr
                  | true, false -> newstr :=   "(" ^ b ^ ")"
                  | false, true -> newstr :=  b ^ " + " ^ !newstr
                  | false, false ->  newstr := b
                )
              done;
              if(!caseZero) then ("(" ^ newstr.contents ^ " + "^ "0" ^")")
              else
                (
                  if (len >= 2) then ( "(" ^ newstr.contents ^")" )
                  else (
                    let strtmp= newstr.contents in
                    (
                      match strtmp with
                        "free" | "malloc" -> strtmp
                      | _ -> "(" ^ strtmp ^")"
                    )
                  )
                )
            end
  )

and fixStmt (fname : string) (s : stmt) : string =
  match s.skind with
  | Instr il ->
    let str = fixinstrs fname il in
    deleteSemi str
  | If(_,tb,fb,_) ->
     let sb1 =deleteSemi( fixBlock fname tb) in
     let sb2 =deleteSemi( fixBlock fname fb) in
     let b1 = findMorF sb1 in
     let b2 = findMorF sb2 in
     begin
     match (b1,b2) with
       (true, true) ->
         (
           let b3 = isContainsSemi sb1  in
           let b4 = isContainsSemi sb2  in
           match(b3, b4)  with
             (true ,true ) ->  "(" ^ "("^ sb1^")" ^  " + "  ^"("^ sb2 ^")" ^ ")"
             | (true, false)->   "(" ^ "("^sb1^")" ^  " + " ^ sb2 ^ ")"
             | (false,true)->  "("^sb1 ^  " + "  ^"("^ sb2 ^")" ^ ")"
             | (false, false) ->  "(" ^sb1 ^  " + "  ^ sb2  ^ ")"
         )
     | (true, false) ->
       ( let nstr= ( if (String.length sb2) >0 then sb2 else "0" ) in
         let b3 = isContainsSemi sb1  in
         (* print_string (sb1 ^"   " ^string_of_bool b3 ^ "   2\n"); *)
         if b3 then   "(" ^ "("^sb1^")" ^  " + " ^ nstr ^ ")" else
           "(" ^sb1 ^  " + "  ^ nstr  ^ ")"
       )
     | (false, true) ->
       ( let nstr =(if (String.length sb1) > 0 then sb1 else "0" ) in
         let b3 = isContainsSemi sb2  in
         if b3 then   "("^ nstr ^  " + "  ^"("^ sb2 ^")" ^ ")"
         else "(" ^  nstr ^  " + "  ^ sb2  ^ ")"
       )
     | (false, false) -> ""
        (* let nstr1 = (if (String.length sb1)>0 then sb1 else "0") in *)
        (* let nstr2 = (if (String.length sb2)>0 then sb2 else "0") in *)
        (* "(" ^ (deleteSemi nstr1) ^  " + "  ^ (deleteSemi nstr2)  ^ ")" *)
     end
  | Switch(_,b, stmts ,_) ->
     caseString := [];
     mergecaselist := [];
     let num = findAllCases fname 0 b.bstmts in
     let newlist = List.rev (caseString.contents) in
     let strnew = mergeCases num newlist in
     mergecaselist := [];
     caseString := [];
     caseZero := false;
     if(findMorF strnew) then strnew else ""
  | Loop(b,_,_,_) ->
     let sb = fixBlock fname b in
     let b = findMorF sb in
     begin
       match b with
         true ->  "(ua." ^ deleteSemi sb ^ ";a)"
       | false -> (* "(ua." ^ "0" ^ ";a)" *) ""
     end
  | Block b ->
     fixBlock fname b
  | TryFinally(b1, b2, _) ->
     "(" ^ deleteSemi (fixBlock fname b1) ^ " f+ " ^ deleteSemi (fixBlock fname b2)^ ")"
  | TryExcept(b1,_,b2,_) ->
     "(" ^ deleteSemi (fixBlock fname b1 ) ^  " try+ "  ^ deleteSemi (fixBlock fname b2 )  ^ ")"
  | Return (None, loc) -> ""
  | Return (Some expr, loc) -> fixExpr fname expr
  | Goto _ -> ""
  | Break _ -> ""
  | Continue _ -> ""
  | _ -> print_string " stms unknown "; ""

and fixstmts (fname) (stmts: stmt list) : string =
  match stmts with
      [] -> ""
    | s:: [] -> (fixStmt fname s)
    | s :: rest ->let str = ( fixStmt fname s) in
                  if (String.length str) > 0 then (str) ^ ";"^ (fixstmts fname rest) else (fixstmts fname rest)

and fixBlock (fname : string) (b : block)  = fixstmts fname b.bstmts

and fixFunction (fd : fundec)  = fixBlock (fd.svar.vname) fd.sbody

and printfuns (flists: (string*string)list) : unit  =
  match flists with
    (a,b) :: rest ->( if ( String.length a) >0 then
                        begin
                          print_string a; print_string ":  "; print_string (deleteSemi b ); print_newline ();
                          printfuns rest
                        end
                      else
                        printfuns rest
                    )
  | [] -> ()

let abstract (f : file) : unit =
  try
    (  List.iter (fun g ->
                  match g with
                  | GFun (fd, loc)  ->
                     if (fd.svar.vname = "main") then
                       ()
                     else (
                       let nstr = fixFunction fd in
                       if !isRecuriveCall then
                         (
                           if(findMorF nstr) then
                             (
                               funclist :=  (fd.svar.vname, "(ua." ^ "(" ^deleteSemi nstr^")" ^")") :: !funclist;
                               isRecuriveCall := false;
                             )
                           else
                             ( funclist :=  (fd.svar.vname, nstr ) :: !funclist;
                              isRecuriveCall := false; )
                         )
                       else
                         (
                           funclist :=  (fd.svar.vname, nstr) :: !funclist
                         )
                     )
                  | _->() )
                 f.globals );

    (  List.iter (fun g ->
                  match g with
                  | GFun (fd, loc) ->
                     if (fd.svar.vname = "main") then
                       funclist :=  (fd.svar.vname, fixFunction fd) :: !funclist
                     else ()
                  | _ -> () )
                 f.globals );

    print_newline ();
    printfuns funclist.contents;
    print_newline ();

  with
    _ -> print_string "Error: abs.ml"; print_newline ()
