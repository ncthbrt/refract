open Refract;

Server.start(
  ~port=9003,
  Request.post
  |. compose(
       Refract.Request.Body.string(body => {
         Js.log(body);
         Refract.Prism.handled;
       }),
     )
  |. compose(
       Path.matches(
         Path.(Constant("hello", String(End))),
         (name, ()) => {
           Js.log(name);
           Refract.Response.Body.string("hello " ++ name);
         },
       ),
     )
  |. compose(Response.status(StatusCode.ImATeapot)),
);