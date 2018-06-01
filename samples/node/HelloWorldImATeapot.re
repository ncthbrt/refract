open Refract;

/*
 open Refract.Operators;

 Server.start(
      ~port=9003,
      Request.get
      >>> Refract.Request.url(url => {
            Js.log(url);
            Refract.Machine.handled;
          })
      >>> Path.onMatch(
            Path.(Constant("hello", String(End))),
            name => {
              Js.log(name);
              Refract.Machine.handled;
            },
          )
      >>> Response.status(StatusCode.ImATeapot),
    );
 */
Server.start(
  ~port=9003,
  Request.get
  |. compose(
       Refract.Request.url(url => {
         Js.log(url);
         Refract.Machine.handled;
       }),
     )
  |. compose(
       Path.onMatch(
         Path.(Constant("hello", String(End))),
         (name, ctx) => {
           Js.log(name);
           Refract.Machine.handled(ctx);
         },
       ),
     )
  |. compose(Response.status(StatusCode.ImATeapot)),
);