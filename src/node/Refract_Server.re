module NodeServer = {
  type t;
  [@bs.module "http"]
  external createServer :
    ((. Refract_Request.t, Refract_Node.Response.t) => unit) => t =
    "createServer";
  [@bs.module "https"]
  external createSecureServer :
    (
      {
        .
        "key": string,
        "cert": string,
      },
      (. Refract_Request.t, Refract_Node.Response.t) => unit
    ) =>
    t =
    "createServer";
};

type t = NodeServer.t;

[@bs.send] external listen : (t, int) => unit = "";

let handler = machine =>
  (. req, res) => {
    let context: Refract_HttpContext.t = {
      request: req,
      response: Refract_Response.empty,
    };
    let result = machine(context);
    ignore(
      Repromise.then_(
        fun
        | Refract_Machine.Unhandled(None) =>
          Repromise.resolve(
            {
              Refract_Node.Response.statusCode(res, 404);
              Refract_Node.Response.end_(res);
            },
          )
        | Unhandled(Some(err)) => {
            Js.Console.error2("Internal Server Error", err);
            Repromise.resolve(
              {
                Refract_Node.Response.statusCode(res, 500);
                Refract_Node.Response.end_(res);
              },
            );
          }
        | Handled(ctx) =>
          Repromise.resolve(
            {
              Refract_Node.Response.statusCode(
                res,
                Refract_StatusCode.toInt(ctx.response.status),
              );
              Refract_Node.Response.end_(res);
            },
          ),
        result,
      ),
    );
  };

let start = (~port=3000, machine) => {
  let server = NodeServer.createServer(handler(machine));
  listen(server, port);
  server;
};

let startSecure = (~privateKey, ~publicKey, ~port=3000, machine) => {
  let server =
    NodeServer.createSecureServer(
      {"key": privateKey, "cert": publicKey},
      handler(machine),
    );
  listen(server, port);
  server;
};