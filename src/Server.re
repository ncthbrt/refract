type t;

let start: (~port: int=?, Machine.t) => t =
  (~port: int=3000, _) => Obj.magic();