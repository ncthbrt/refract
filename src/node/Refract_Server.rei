type t;

let start: (~port: int=?, Refract_Machine.t) => t;

let startSecure:
  (~privateKey: string, ~publicKey: string, ~port: int=?, Refract_Machine.t) =>
  t;