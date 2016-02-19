type 'a ref_ = {mutable valeur : 'a}

let ref_fun arg = {valeur = arg};;

let modif arg_ref val_new =  arg_ref.valeur <- val_new;;

let deref arg_ref =  arg_ref.valeur;;
