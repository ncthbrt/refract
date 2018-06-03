module StatusCode = Refract_StatusCode;

module Json = Refract_Json;

module Method = Refract_Method;

module HttpContext = Refract_HttpContext;

module Prism = Refract_Prism;

module Path = Refract_Path;

module Query = Refract_Query;

module Request = {
  module Body = {
    let string = (f: string => Prism.t, ctx: HttpContext.t) => {
      let str = Refract_Request.Body.string(ctx.request);
      Repromise.then_(str => f(str, ctx), str);
    };
  };
  let method: (Method.t => Prism.t) => Prism.t =
    (f, ctx) => f(Refract_Request.method_(ctx.request), ctx);
  let isMethod: Method.t => Prism.t =
    (method, ctx) =>
      Refract_Request.method_(ctx.request) == method ?
        Prism.handled(ctx) : Prism.unhandled(ctx);
  let get = isMethod(Method.Get);
  let post = isMethod(Method.Post);
  let put = isMethod(Method.Put);
  let patch = isMethod(Method.Patch);
  let delete = isMethod(Method.Delete);
  let options = isMethod(Method.Options);
  let url: (string => Prism.t) => Prism.t =
    (f, ctx) => f(Refract_Request.url(ctx.request), ctx);
};

module Response = {
  let ok: Prism.t =
    ctx =>
      Prism.handled({
        ...ctx,
        response: Refract_Response.status(ctx.response, StatusCode.Ok),
      });
  let notFound: Prism.t =
    ctx =>
      Prism.handled({
        ...ctx,
        response: Refract_Response.status(ctx.response, StatusCode.NotFound),
      });
  let status: StatusCode.t => Prism.t =
    (status, ctx) =>
      Prism.handled({
        ...ctx,
        response: Refract_Response.status(ctx.response, status),
      });
  module Body = {
    let string: string => Prism.t =
      (str, ctx) =>
        Prism.handled({
          ...ctx,
          response: Refract_Response.Body.string(ctx.response, str),
        });
  };
};

let map = (res: Repromise.t(Prism.result), f) =>
  Repromise.then_(
    fun
    | Prism.Handled(ctx) => f(ctx)
    | Unhandled(err) => Repromise.resolve(Prism.Unhandled(err)),
    res,
  );

let mapUnhandled = (res, f) =>
  Repromise.then_(
    fun
    | Prism.Unhandled(e) => f(e)
    | Handled(ctx) => Repromise.resolve(Prism.Handled(ctx)),
    res,
  );

let flatMap = (res: Repromise.t(Prism.result), f: Prism.t) =>
  Repromise.then_(
    fun
    | Prism.Handled(ctx) => f(ctx)
    | Unhandled(e) => Repromise.resolve(Prism.Unhandled(e)),
    res,
  );

let bind = flatMap;

let then_ = flatMap;

let compose: (Prism.t, Prism.t) => Prism.t =
  (a, b, ctx) => flatMap(a(ctx), b);

let rec composeMany: list(Prism.t) => Prism.t =
  (prisms, ctx) =>
    switch (prisms) {
    | [] => Prism.unhandled(ctx)
    | [prism] => prism(ctx)
    | [prism, ...prisms] => compose(prism, composeMany(prisms), ctx)
    };

let switch_: list(Prism.t) => Prism.t =
  (lst, ctx: HttpContext.t) => {
    let rec aux =
      fun
      | [] => Prism.unhandled(ctx)
      | [hd, ...tail] =>
        mapUnhandled(hd(ctx), x =>
          switch (x) {
          | None => aux(tail)
          | Some(e) => Repromise.resolve(Prism.Unhandled(Some(e)))
          }
        );
    aux(lst);
  };

let match_ = switch_;

let zip:
  (
    ('a => Prism.t) => Prism.t,
    ('b => Prism.t) => Prism.t,
    ('a, 'b) => Prism.t
  ) =>
  Prism.t =
  (ma, mb, f, ctx) => ma(a => mb(f(a)), ctx);

let convolute = zip;

module Server = Refract_Server;