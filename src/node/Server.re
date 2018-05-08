type t;

let start: (~port: int=?, Machine.t) => t =
  (~port=3000, machine) => Obj.magic();