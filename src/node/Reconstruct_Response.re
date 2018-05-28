type t = {
  headers: list((string, string)),
  status: Reconstruct_StatusCode.t,
  body: Reconstruct_Body.t,
};

let empty = {
  headers: [],
  status: Reconstruct_StatusCode.NotFound,
  body: `Empty,
};

let status = (res, statusCode: Reconstruct_StatusCode.t) => {
  ...res,
  status: statusCode,
};