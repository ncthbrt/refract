module StatusCode = Refract_StatusCode;

module Json = Refract_Json;

module Method = Refract_Method;

module HttpContext = Refract_HttpContext;

module Machine = Refract_Machine;

module Path = Refract_Path;

module Query = Refract_Query;

module Request = {
  let body = (decoder, f, ctx: HttpContext.t) =>
    Repromise.then_(body => f(body, ctx), decoder(ctx));
  let method: (Method.t => Machine.t) => Machine.t =
    (f, ctx) => f(Refract_Request.method_(ctx.request), ctx);
  let isMethod: Method.t => Machine.t =
    (method, ctx) =>
      Refract_Request.method_(ctx.request) == method ?
        Machine.handled(ctx) : Machine.unhandled(ctx);
  let get = isMethod(Method.Get);
  let post = isMethod(Method.Post);
  let put = isMethod(Method.Put);
  let patch = isMethod(Method.Patch);
  let delete = isMethod(Method.Delete);
};

module Response = {
  let ok: Machine.t =
    ctx =>
      Machine.handled({
        ...ctx,
        response: Refract_Response.status(ctx.response, StatusCode.Ok),
      });
  let notFound: Machine.t =
    ctx =>
      Machine.handled({
        ...ctx,
        response: Refract_Response.status(ctx.response, StatusCode.NotFound),
      });
  let status: StatusCode.t => Machine.t =
    (status, ctx) =>
      Machine.handled({
        ...ctx,
        response: Refract_Response.status(ctx.response, status),
      });
};

let map = (res: Repromise.t(Machine.result), f) =>
  Repromise.then_(
    fun
    | Machine.Handled(ctx) => f(ctx)
    | Unhandled(err) => Repromise.resolve(Machine.Unhandled(err)),
    res,
  );

let mapUnhandled = (res, f) =>
  Repromise.then_(
    fun
    | Machine.Unhandled(e) => f(e)
    | Handled(ctx) => Repromise.resolve(Machine.Handled(ctx)),
    res,
  );

let flatMap = (res: Repromise.t(Machine.result), f: Machine.t) =>
  Repromise.then_(
    fun
    | Machine.Handled(ctx) => f(ctx)
    | Unhandled(e) => Repromise.resolve(Machine.Unhandled(e)),
    res,
  );

let bind = flatMap;

let compose: (Machine.t, Machine.t) => Machine.t =
  (a, b, ctx) => flatMap(a(ctx), b);

let switch_: list(Machine.t) => Machine.t =
  (lst, ctx: HttpContext.t) => {
    let rec aux =
      fun
      | [] => Machine.unhandled(ctx)
      | [hd, ...tail] =>
        mapUnhandled(hd(ctx), x =>
          switch (x) {
          | None => aux(tail)
          | Some(e) => Repromise.resolve(Machine.Unhandled(Some(e)))
          }
        );
    aux(lst);
  };

let match_ = switch_;

let zip:
  (
    ('a => Machine.t) => Machine.t,
    ('b => Machine.t) => Machine.t,
    ('a, 'b) => Machine.t
  ) =>
  Machine.t =
  (ma, mb, f, ctx) => ma(a => mb(f(a)), ctx);

let convolute = zip;

module Operators = {
  let (|>|) = flatMap;
  let (>>>) = compose;
  let (^@) = zip;
};

module Server = Refract_Server;