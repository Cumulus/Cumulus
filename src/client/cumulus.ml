module type Null = sig end

let _ = (module Templates : Null)

(** Just a value to keep this module in the .js file even if there
    is no explicit calls *)
let _link = ()