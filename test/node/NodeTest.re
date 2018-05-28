open Reconstruct;

open Reconstruct.Operators;

let machine = Request.get >>> Response.status(StatusCode.ImATeapot);

Server.start(~port=9001, machine);