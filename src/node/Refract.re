include RefractCommon.Make(RefractNodeRequest, RefractString);

module Server: {
  type t;
  let start: (~port: int=?, Prism.t) => t;
  let startSecure:
    (~privateKey: string, ~publicKey: string, ~port: int=?, Prism.t) =>
    t;
} = {
  module NodeServer = {
    type t;
    [@bs.module "http"]
    external createServer :
      ((. RefractNodeRequest.t, RefractNode.Response.t) => unit) => t =
      "createServer";
    [@bs.module "https"]
    external createSecureServer :
      (
        {
          .
          "key": string,
          "cert": string,
        },
        (. RefractNodeRequest.t, RefractNode.Response.t) => unit
      ) =>
      t =
      "createServer";
  };
  type t = NodeServer.t;
  [@bs.send] external listen : (t, int) => unit = "";
  let toResponse = (ctx: HttpContext.t, nodeRes) => {
    RefractNode.Response.writeHead(
      nodeRes,
      StatusCode.toInt(ctx.response.status),
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
          RefractNode.Response.end_(nodeRes);
          ();
        },
      )
    | `String(str) =>
      let (promise: Repromise.t(unit), resolve) = Repromise.new_();
      RefractNode.Response.write(
        nodeRes,
        str,
        `utf8,
        () => {
          RefractNode.Response.end_(nodeRes);
          resolve();
          ();
        },
      );
      promise;
    | `Stream(f) =>
      f(str => {
        let (promise: Repromise.t(unit), resolve) = Repromise.new_();
        RefractNode.Response.write(nodeRes, str, `utf8, resolve);
        promise;
      })
      |. Repromise.then_(
           () => {
             RefractNode.Response.end_(nodeRes);
             Repromise.resolve();
           },
           _,
         )
    };
  };
  let handler = prism =>
    (. req, res) => {
      let context: HttpContext.t = {
        request: req,
        response: RefractCommon.RefractResponse.empty,
      };
      let result = prism(context);
      ignore(
        Repromise.then_(
          fun
          | Prism.Unhandled(None) =>
            Repromise.resolve(
              {
                RefractNode.Response.statusCode(res, 404);
                RefractNode.Response.end_(res);
              },
            )
          | Unhandled(Some(err)) => {
              Js.Console.error2("Internal Server Error", err);
              Repromise.resolve(
                {
                  RefractNode.Response.statusCode(res, 500);
                  RefractNode.Response.end_(res);
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
};