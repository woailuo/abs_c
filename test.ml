
module Env =
  functor (file : Cil.file) ->
  struct
    
    let f = fun x -> let x = x + 1 in print_int x ;;

    let _ = f 3 ; print_newline ()
  end
