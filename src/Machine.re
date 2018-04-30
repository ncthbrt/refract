type result =
  | Unhandled(option(exn))
  | Handled(HttpContext.t);

type t = HttpContext.t => Repromise.t(result);

let handled: t = ctx => Repromise.resolve(Handled(ctx));

let unhandled: t = ctx => Repromise.resolve(Unhandled(None));

let unhandledWithError: exn => t =
  (err, ctx) => Repromise.resolve(Unhandled(Some(err)));