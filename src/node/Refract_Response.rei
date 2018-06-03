type t = {
  headers: list((string, string)),
  status: Refract_StatusCode.t,
  body: Refract_Body.t,
};

let empty: t;

let status: (t, Refract_StatusCode.t) => t;

module Body: {let string: (t, string) => t;};