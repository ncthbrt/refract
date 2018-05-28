type t = {
  headers: list((string, string)),
  status: Reconstruct_StatusCode.t,
  body: Reconstruct_Body.t,
};

let empty: t;

let status: (t, Reconstruct_StatusCode.t) => t;