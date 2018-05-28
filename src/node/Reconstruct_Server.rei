type t;

let start: (~port: int=?, Reconstruct_Machine.t) => t;

let startSecure:
  (
    ~privateKey: string,
    ~publicKey: string,
    ~port: int=?,
    Reconstruct_Machine.t
  ) =>
  t;