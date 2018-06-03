type result =
  | Unhandled(option(exn))
  | Handled(Refract_HttpContext.t);

type t = Refract_HttpContext.t => Repromise.t(result);

let handled: t = ctx => Repromise.resolve(Handled(ctx));

let unhandled: t =
  (_: Refract_HttpContext.t) => Repromise.resolve(Unhandled(None));

let unhandledWithError: exn => t =
  (err, _) => Repromise.resolve(Unhandled(Some(err)));