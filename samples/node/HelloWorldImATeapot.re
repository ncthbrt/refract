open Refract;

open Refract.Operators;

Server.start(
  ~port=9003,
  Request.get
  >>> Refract.Request.url(url => {
        Js.log(url);
        Refract.Machine.handled;
      })
  >>> Path.matches(
        Path.(Constant("hello", End)),
        ctx => {
          Js.log("matched");
          Refract.Machine.handled(ctx);
        },
      )
  >>> Response.status(StatusCode.ImATeapot),
);