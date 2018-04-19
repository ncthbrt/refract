[%route "/hello/%s"]("hello");

(ctx: Reconstruct.HttpContext.t, f) =>
  Reconstruct.Route.evaluate(
    ctx.request,
    [Reconstruct.Route.Constant("hello"), Reconstruct.Route.String],
  );