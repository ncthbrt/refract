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
        Path.(Constant("hello", String("name", End))),
        (name, ctx) => {
          Js.log(name);
          Refract.Machine.handled(ctx);
        },
      )
  >>> Response.status(StatusCode.ImATeapot),
);