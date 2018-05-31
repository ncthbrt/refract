open Refract;

open Refract.Operators;

Server.start(
  ~port=9001,
  Request.get >>> Response.status(StatusCode.ImATeapot),
);