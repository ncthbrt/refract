type t;

module Bindings = {
  [@bs.module "http2"]
  external createServer : ((Request.t, Response.t) => unit) => t = "";
  [@bs.module "http2"]
  external createSecureServer :
    (
      {
        .
        "key": string,
        "cert": string,
      },
      (Request.t, Response.t) => unit
    ) =>
    t =
    "";
};

[@bs.send] external listen : (t, int) => unit = "";

let handler = (machine, req, res) => {
  let context: HttpContext.t = {request: req, response: res};
  let result = machine(context);
  ignore(
    Repromise.then_(
      fun
      | Machine.Unhandled(None) => Repromise.resolve()
      | Unhandled(Some(err)) => Repromise.resolve()
      | Handled(ctx) => Repromise.resolve(),
      result,
    ),
  );
};

let start = (~port=3000, machine) => {
  let server = Bindings.createServer(handler(machine));
  listen(server, port);
  server;
};

let startSecure = (~privateKey, ~publicKey, ~port=3000, machine) => {
  let server =
    Bindings.createSecureServer(
      {"key": privateKey, "cert": publicKey},
      handler(machine),
    );
  listen(server, port);
  server;
};