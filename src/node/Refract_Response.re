type t = {
  headers: list((string, string)),
  status: Refract_StatusCode.t,
  body: Refract_Body.t,
};

let empty = {headers: [], status: Refract_StatusCode.NotFound, body: `Empty};

let status = (res, statusCode: Refract_StatusCode.t) => {
  ...res,
  status: statusCode,
};

module Body = {
  let string: (t, string) => t = (res, str) => {...res, body: `String(str)};
};