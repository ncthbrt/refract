module NodeServer = {
  type t;
  [@bs.module "http"]
  external createServer :
    ((. Reconstruct_Request.t, Reconstruct_Node.Response.t) => unit) => t =
    "createServer";
  [@bs.module "https"]
  external createSecureServer :
    (
      {
        .
        "key": string,
        "cert": string,
      },
      (. Reconstruct_Request.t, Reconstruct_Node.Response.t) => unit
    ) =>
    t =
    "createServer";
};

type t = NodeServer.t;

[@bs.send] external listen : (t, int) => unit = "";

let handler = machine =>
  (. req, res) => {
    Js.log("received a request");
    let context: Reconstruct_HttpContext.t = {
      request: req,
      response: Reconstruct_Response.empty,
    };
    let result = machine(context);
    ignore(
      Repromise.then_(
        fun
        | Reconstruct_Machine.Unhandled(None) =>
          Repromise.resolve(
            {
              Reconstruct_Node.Response.statusCode(res, 404);
              Reconstruct_Node.Response.end_(res);
            },
          )
        | Unhandled(Some(err)) =>
          Repromise.resolve(
            {
              Reconstruct_Node.Response.statusCode(res, 500);
              Reconstruct_Node.Response.end_(res);
            },
          )
        | Handled(ctx) =>
          Repromise.resolve(
            {
              Reconstruct_Node.Response.statusCode(
                res,
                Reconstruct_StatusCode.toInt(ctx.response.status),
              );
              Reconstruct_Node.Response.end_(res);
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