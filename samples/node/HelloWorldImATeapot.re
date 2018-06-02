open Refract;

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
       Path.matches(
         Path.(Constant("hello", String(Int(End))),
         (name, ()) => {
           Js.log(name);
           Refract.Machine.handled;
         },
       ),
     )
  |. compose(Response.status(StatusCode.ImATeapot)),
);