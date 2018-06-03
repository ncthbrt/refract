type t;

let start: (~port: int=?, Refract_Prism.t) => t;

let startSecure:
  (~privateKey: string, ~publicKey: string, ~port: int=?, Refract_Prism.t) => t;