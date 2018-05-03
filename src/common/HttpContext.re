type t = {
  request: Request.t,
  response: Response.t,
};

module Request = {
  let method = (ctx: t) => ctx.request.method;
};