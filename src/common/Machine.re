type result =
  | Unhandled(option(exn))
  | Handled(HttpContext.t);

type t = HttpContext.t => Repromise.t(result);

let handled: t = ctx => Repromise.resolve(Handled(ctx));

let unhandled: t = (_: HttpContext.t) => Repromise.resolve(Unhandled(None));

let unhandledWithError: exn => t =
  (err, _) => Repromise.resolve(Unhandled(Some(err)));