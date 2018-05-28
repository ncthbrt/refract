type result =
  | Unhandled(option(exn))
  | Handled(Reconstruct_HttpContext.t);

type t = Reconstruct_HttpContext.t => Repromise.t(result);

let handled: t = ctx => Repromise.resolve(Handled(ctx));

let unhandled: t =
  (_: Reconstruct_HttpContext.t) => Repromise.resolve(Unhandled(None));

let unhandledWithError: exn => t =
  (err, _) => Repromise.resolve(Unhandled(Some(err)));