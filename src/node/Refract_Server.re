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

let toResponse = (ctx: Refract_HttpContext.t, nodeRes) => {
  Refract_Node.Response.writeHead(
    nodeRes,
    Refract_StatusCode.toInt(ctx.response.status),
    ctx.response.headers
    |. Belt.List.reduce(
         Js.Dict.empty(),
         (prev, (k, v)) => {
           Js.Dict.set(prev, k, v);
           prev;
         },
       ),
  );
  switch (ctx.response.body) {
  | `Empty =>
    Repromise.resolve(
      {
        Refract_Node.Response.end_(nodeRes);
        ();
      },
    )
  | `String(str) =>
    let (promise: Repromise.t(unit), resolve) = Repromise.new_();
    Refract_Node.Response.write(
      nodeRes,
      str,
      `utf8,
      () => {
        Refract_Node.Response.end_(nodeRes);
        resolve();
        ();
      },
    );
    promise;
  | `Stream(f) =>
    f(str => {
      let (promise: Repromise.t(unit), resolve) = Repromise.new_();
      Refract_Node.Response.write(nodeRes, str, `utf8, resolve);
      promise;
    })
    |. Repromise.then_(
         () => {
           Refract_Node.Response.end_(nodeRes);
           Repromise.resolve();
         },
         _,
       )
  };
};

let handler = prism =>
  (. req, res) => {
    let context: Refract_HttpContext.t = {
      request: req,
      response: Refract_Response.empty,
    };
    let result = prism(context);
    ignore(
      Repromise.then_(
        fun
        | Refract_Prism.Unhandled(None) =>
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
        | Handled(ctx) => toResponse(ctx, res),
        result,
      ),
    );
  };

let start = (~port=3000, prism) => {
  let server = NodeServer.createServer(handler(prism));
  listen(server, port);
  server;
};

let startSecure = (~privateKey, ~publicKey, ~port=3000, prism) => {
  let server =
    NodeServer.createSecureServer(
      {"key": privateKey, "cert": publicKey},
      handler(prism),
    );
  listen(server, port);
  server;
};