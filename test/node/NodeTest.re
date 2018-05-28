open Reconstruct;

open Reconstruct.Operators;

Server.start(
  ~port=9001,
  Request.get >>> Response.status(StatusCode.ImATeapot),
);