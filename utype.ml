open Cil

type funRecord =
  {mutable fName: string; mutable bType: string; mutable funbody : fundec}

let printFlag = false

let prints str  =
  if printFlag = true then print_string str
  else  ()

