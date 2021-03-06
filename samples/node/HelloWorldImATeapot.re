open Refract;

Server.start(
  ~port=9003,
  Request.post
  |. compose(
       Request.Body.string(body => {
         Js.log(body);
         Refract.Prism.handled;
       }),
     )
  |. compose(
       Request.Pathname.(
         ctx =>
           matches(
             Constant("hello", String(End)),
             (name, ()) => {
               Js.log(name);
               Refract.Response.Body.string("hello " ++ name);
             },
             ctx,
           )
       ),
     )
  |. compose(Response.status(StatusCode.ImATeapot)),
);