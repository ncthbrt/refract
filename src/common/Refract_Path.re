exception RouteDoesNotMatch;

exception MalformedQueryParameter(string, string, exn);

type t('func, 'result) =
  | End: t('result, 'result)
  | Constant(string, t('func, 'result)): t('func, 'result)
  | String(string, t('func, 'result)): t(string => 'func, 'result)
  | Int(string, t('func, 'result)): t(int => 'func, 'result)
  | Float(string, t('func, 'result)): t(float => 'func, 'result)
  | Wildcard(t('func, 'result)): t('func, 'result)
  | Custom(string, string => 'a, t('func, 'result)): t('a => 'func, 'result);

let split: Refract_HttpContext.t => list(string) =
  ctx =>
    Refract_CrossplatString.splitOnChar(
      '/',
      Refract_Request.url(ctx.request),
    );

let rec evalPath:
  type func result. (func, t(func, result), list(string)) => result =
  (f, route, parts) =>
    switch (route, parts) {
    | (End, []) => f
    | (_, []) => raise(RouteDoesNotMatch)
    | (End, _) => raise(RouteDoesNotMatch)
    | (Constant(value, tl), [str, ...next]) when value == str =>
      evalPath(f, tl, next)
    | (Constant(_), _) => raise(RouteDoesNotMatch)
    | (String(_, tl), [str, ...next]) => evalPath(f(str), tl, next)
    | (Int(_, tl), [str, ...next]) =>
      let value =
        try (int_of_string(str)) {
        | Failure(_) => raise(RouteDoesNotMatch)
        };
      evalPath(f(value), tl, next);
    | (Float(_, tl), [str, ...next]) =>
      let value =
        try (float_of_string(str)) {
        | Failure(_) => raise(RouteDoesNotMatch)
        };
      evalPath(f(value), tl, next);
    | (Wildcard(tl), [_, ...next]) =>
      try (evalPath(f, tl, next)) {
      | RouteDoesNotMatch => evalPath(f, Wildcard(tl), next)
      }
    | (Custom(_, parser, tl), [str, ...next]) =>
      let value =
        try (parser(str)) {
        | _ => raise(RouteDoesNotMatch)
        };
      evalPath(f(value), tl, next);
    };

let matches:
  type func. (t(func, Refract_Machine.t), func) => Refract_Machine.t =
  (path, f, ctx) => {
    let pathParts = split(ctx);
    (
      try (evalPath(f, path, pathParts)) {
      | RouteDoesNotMatch => Refract_Machine.unhandled
      }
    )(
      ctx,
    );
  };