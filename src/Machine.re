type result =
  | Unhandled
  | Handled(HttpContext.t);

type t = HttpContext.t => Repromise.t(result);

let handled: t = ctx => Repromise.resolve(Handled(ctx));

let unhandled: t = ctx => Repromise.resolve(Unhandled);