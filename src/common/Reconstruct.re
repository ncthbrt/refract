module HttpContext = HttpContext;

module Machine = Machine;

module Request = {
  include Request;
  let body = (decoder, f, ctx: HttpContext.t) =>
    Repromise.then_(body => f(body, ctx), decoder(ctx));
  let bodyString: (string => Machine.t) => Machine.t =
    f => body((_) => Repromise.resolve("HERROR"), f);
};

module Response = {
  include Response;
};

module Route = Route;

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

let method: (Request.Method.t => Machine.t) => Machine.t =
  (f, ctx) => f(ctx.request.method, ctx);

let isMethod: Request.Method.t => Machine.t =
  (method, ctx) =>
    ctx.request.method == method ?
      Machine.handled(ctx) : Machine.unhandled(ctx);

let get = isMethod(Request.Method.Get);

let post = isMethod(Request.Method.Post);

let put = isMethod(Request.Method.Put);

let patch = isMethod(Request.Method.Patch);

let delete = isMethod(Request.Method.Delete);

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
  let (=|>) = flatMap;
  let (=>>) = compose;
  let (^@) = (ma, mb) => zip(ma, mb);
};