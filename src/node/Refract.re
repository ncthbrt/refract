module RefractString = {
  let uppercaseAsciiChar = c =>
    if (c >= 'a' && c <= 'z') {
      Char.unsafe_chr(Char.code(c) - 32);
    } else {
      c;
    };
  let uppercaseAscii = String.map(uppercaseAsciiChar);
  let splitOnChar = (chr, str) =>
    Js.String.split(String.make(1, chr), str) |. Belt.List.fromArray;
};

module Bindings = {
  module Request = {
    type t;
    [@bs.get] external methodAsStr : t => string = "method";
    [@bs.get] external url : t => string = "";
    [@bs.get] external httpVersion : t => string = "";
    [@bs.get] external rawHeaders : t => array(string) = "";
    [@bs.send]
    external on :
      (
        t,
        [@bs.string] [
          | `data(Node.buffer => unit)
          | `error(unit => unit)
          | [@bs.as "end"] `end_(unit => unit)
        ]
      ) =>
      unit =
      "";
  };
  module Response = {
    type t;
    [@bs.send] external setHeader : (t, string, string) => unit = "";
    [@bs.set] external statusCode : (t, int) => unit = "";
    [@bs.set] external statusMessage : (t, string) => unit = "";
    [@bs.send]
    external write : (t, string, [@bs.string] [ | `utf8], unit => unit) => unit =
      "write";
    [@bs.send]
    external writeHead : (t, int, Js.Dict.t(string)) => unit = "writeHead";
    [@bs.send] external end_ : t => unit = "end";
  };
  module Buffer = {
    [@bs.val "Buffer.concat"]
    external concat : array(Node.Buffer.t) => Node.Buffer.t = "";
  };
};

module RefractNodeRequest = {
  type t = Bindings.Request.t;
  let method_ = req =>
    RefractCommon.Method.fromString(
      Bindings.Request.methodAsStr(req) |. Js.String.toUpperCase,
    )
    |. Belt.Option.getExn;
  let headers = req => {
    let rec aux = (prev, headers) =>
      switch (headers) {
      | [k, v, ...tail] => aux([(k, v), ...prev], tail)
      | [] => prev
      | [k] => [(k, ""), ...prev]
      };
    aux([], Bindings.Request.rawHeaders(req) |. Belt.List.fromArray);
  };
  let httpVersion = req => Bindings.Request.httpVersion(req);
  let url = req => Bindings.Request.url(req);
  module Body = {
    let string = req => {
      let (promise: Repromise.t(_), resolve) = Repromise.new_();
      let body = [||];
      Bindings.Request.on(
        req,
        `data(buffer => ignore(Js.Array.push(buffer, body))),
      );
      Bindings.Request.on(req, `error((_) => ()));
      Bindings.Request.on(
        req,
        `end_(
          (_) =>
            resolve(Bindings.Buffer.concat(body) |. Node.Buffer.toString),
        ),
      );
      promise;
    };
  };
};

include RefractCommon.Make(RefractNodeRequest, RefractString);

module Server = {
  module NodeServer = {
    type t;
    [@bs.module "http"]
    external createServer :
      ((. RefractNodeRequest.t, Bindings.Response.t) => unit) => t =
      "createServer";
    [@bs.module "https"]
    external createSecureServer :
      (
        {
          .
          "key": string,
          "cert": string,
        },
        (. RefractNodeRequest.t, Bindings.Response.t) => unit
      ) =>
      t =
      "createServer";
  };
  type t = NodeServer.t;
  [@bs.send] external listen : (t, int) => unit = "";
  let toResponse = (ctx: HttpContext.t, nodeRes) => {
    Bindings.Response.writeHead(
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
          Bindings.Response.end_(nodeRes);
          ();
        },
      )
    | `String(str) =>
      let (promise: Repromise.t(unit), resolve) = Repromise.new_();
      Bindings.Response.write(
        nodeRes,
        str,
        `utf8,
        () => {
          Bindings.Response.end_(nodeRes);
          resolve();
        },
      );
      promise;
    | `Stream(f) =>
      f(str => {
        let (promise: Repromise.t(unit), resolve) = Repromise.new_();
        Bindings.Response.write(nodeRes, str, `utf8, resolve);
        promise;
      })
      |. Repromise.then_(
           () => {
             Bindings.Response.end_(nodeRes);
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
                Bindings.Response.statusCode(res, 404);
                Bindings.Response.end_(res);
              },
            )
          | Unhandled(Some(err)) => {
              Js.Console.error2("Internal Server Error", err);
              Repromise.resolve(
                {
                  Bindings.Response.statusCode(res, 500);
                  Bindings.Response.end_(res);
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