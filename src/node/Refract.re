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
  let splitFirst = (chr, str) => {
    let index = Js.String.indexOf(String.make(1, chr), str);
    if (index > 0) {
      (
        Js.String.substring(~from=0, ~to_=index, str),
        Some(Js.String.substr(~from=index, str)),
      );
    } else {
      (str, None);
    };
  };
};

module Bindings = {
  module Request = {
    type t;
    [@bs.get] external methodAsStr : t => string = "method";
    [@bs.get] external url : t => string = "";
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
    [@bs.set] external statusCode : (t, int) => unit = "";
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
  module Server = {
    type t;
    [@bs.module "http"]
    external createServer : ((. Request.t, Response.t) => unit) => t =
      "createServer";
    [@bs.module "https"]
    external createSecureServer :
      (
        {
          .
          "key": string,
          "cert": string,
        },
        (. Request.t, Response.t) => unit
      ) =>
      t =
      "createServer";
  };
  module Url = {
    type t;
    [@bs.module "url"] [@bs.val] external parse : string => t = "parse";
    [@bs.get] external pathname : t => string = "pathname";
    [@bs.get] external query : t => string = "query";
  };
};

module RefractJson = {
  exception JsonParseError(string);
  type t = Js.Json.t;
  type encoder('a) = 'a => t;
  type decoder('a) = t => 'a;
  module Decoder = {
    exception DecodeError(string, Js.Json.t, option(exn));
    let null: decoder(unit) =
      (json: Js.Json.t) =>
        Js.Json.null == json ?
          () :
          raise(
            DecodeError(
              "excepted null got: " ++ Js.Json.stringify(json),
              json,
              None,
            ),
          );
    let bool = (json: Js.Json.t) =>
      if (Js.typeof(json) == "boolean") {
        (Obj.magic(json): bool);
      } else {
        raise(
          DecodeError(
            "expected boolean, got " ++ Js.Json.stringify(json),
            json,
            None,
          ),
        );
      };
    let string = (json: Js.Json.t) =>
      if (Js.typeof(json) == "string") {
        (Obj.magic(json): string);
      } else {
        raise(
          DecodeError(
            "expected string, got " ++ Js.Json.stringify(json),
            json,
            None,
          ),
        );
      };
    let float = (json: Js.Json.t) =>
      if (Js.typeof(json) == "number") {
        (Obj.magic(json): float);
      } else {
        raise(
          DecodeError(
            "expected float, got " ++ Js.Json.stringify(json),
            json,
            None,
          ),
        );
      };
    let assoc = (innerDecoder, json) =>
      if (Js.typeof(json) == "object"
          && ! Js.Array.isArray(json)
          && ! ((Obj.magic(json): Js.null('a)) == Js.null)) {
        let jsDict: Js.Dict.t(Js.Json.t) = Obj.magic(json);
        try (
          Js.Dict.entries(jsDict)
          |. Belt.List.fromArray
          |. Belt.List.map(((k, v)) => (k, innerDecoder(v)))
        ) {
        | DecodeError(_, _, _) as e =>
          raise(DecodeError("failed to decode assoc value", json, Some(e)))
        };
      } else {
        raise(
          DecodeError(
            "expected array, got " ++ Js.Json.stringify(json),
            json,
            None,
          ),
        );
      };
    let list = (innerDecoder, json) =>
      if (Js.Array.isArray(json)) {
        let jsonArr: array(Js.Json.t) = Obj.magic(json);
        try (jsonArr |. Belt.List.fromArray |. Belt.List.map(innerDecoder)) {
        | DecodeError(_, _, _) as e =>
          raise(DecodeError("failed to decode list value", json, Some(e)))
        };
      } else {
        raise(
          DecodeError(
            "expected array, got " ++ Js.Json.stringify(json),
            json,
            None,
          ),
        );
      };
  };
  let toString = Js.Json.stringify;
  let fromString = json =>
    try (Js.Json.parseExn(json)) {
    | Js.Exn.Error(e) =>
      raise(JsonParseError(Js.Exn.message(e) |. Belt.Option.getExn))
    };
};

module RefractRequest = {
  type t = {
    innerRequest: Bindings.Request.t,
    mutable body: option(Node.Buffer.t),
    mutable parsedUrl: option(Bindings.Url.t),
    mutable parsedPathname: option(list(string)),
    mutable parsedQuery: option(list((string, option(string)))),
  };
  let make = innerRequest => {
    innerRequest,
    body: None,
    parsedUrl: None,
    parsedPathname: None,
    parsedQuery: None,
  };
  let method_ = ({innerRequest}) =>
    RefractCommon.Method.fromString(
      Bindings.Request.methodAsStr(innerRequest) |. Js.String.toUpperCase,
    )
    |. Belt.Option.getExn;
  let headers = ({innerRequest}) => {
    let rec aux = (prev, headers) =>
      switch (headers) {
      | [k, v, ...tail] => aux([(k, v), ...prev], tail)
      | [] => prev
      | [k] => [(k, ""), ...prev]
      };
    aux(
      [],
      Bindings.Request.rawHeaders(innerRequest) |. Belt.List.fromArray,
    );
  };
  let url = req =>
    switch (req.parsedUrl) {
    | Some(url) => url
    | None =>
      let url = Bindings.Request.url(req.innerRequest);
      let parsedUrl = Bindings.Url.parse(url);
      req.parsedUrl = Some(parsedUrl);
      parsedUrl;
    };
  let query = req =>
    switch (req.parsedQuery) {
    | Some(query) => query
    | None =>
      let url = url(req);
      let query = Bindings.Url.query(url);
      let parsedQuery =
        if (query == "") {
          [];
        } else {
          let query =
            List.map(
              x => RefractString.splitFirst('=', x),
              RefractString.splitOnChar('&', query),
            );
          query;
        };
      req.parsedQuery = Some(parsedQuery);
      parsedQuery;
    };
  let pathname = req =>
    switch (req.parsedPathname) {
    | Some(p) => p
    | None =>
      let url = url(req);
      let pathname = Bindings.Url.pathname(url);
      let parsedPathname = RefractString.splitOnChar('/', pathname);
      req.parsedPathname = Some(parsedPathname);
      parsedPathname;
    };
  module Body = {
    let string = req => {
      let (promise: Repromise.t(_), resolve) = Repromise.new_();
      switch (req.body) {
      | Some(body) => resolve(`Ok(body |. Node.Buffer.toString))
      | None =>
        let body = [||];
        Bindings.Request.on(
          req.innerRequest,
          `data(buffer => ignore(Js.Array.push(buffer, body))),
        );
        Bindings.Request.on(req.innerRequest, `error((_) => ()));
        Bindings.Request.on(
          req.innerRequest,
          `end_(
            (_) => {
              let buffer = Bindings.Buffer.concat(body);
              req.body = Some(buffer);
              resolve(`Ok(buffer |. Node.Buffer.toString));
            },
          ),
        );
      };
      promise;
    };
  };
};

include RefractCommon.Make(RefractRequest, RefractString, RefractJson);

module Server = {
  type t = Bindings.Server.t;
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
      let request = RefractRequest.make(req);
      let context: HttpContext.t = HttpContext.make(request);
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
    let server = Bindings.Server.createServer(handler(prism));
    listen(server, port);
    server;
  };
  let startSecure = (~privateKey, ~publicKey, ~port=3000, prism) => {
    let server =
      Bindings.Server.createSecureServer(
        {"key": privateKey, "cert": publicKey},
        handler(prism),
      );
    listen(server, port);
    server;
  };
};