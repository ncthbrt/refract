exception RouteDoesNotMatch;

exception MalformedQueryParameter(string, string, exn);

type t('func, 'result) =
  | End: t(unit => 'result, 'result)
  | Constant(string, t('func, 'result)): t('func, 'result)
  | String(t('func, 'result)): t(string => 'func, 'result)
  | UInt(t('func, 'result)): t(int => 'func, 'result)
  | Int(t('func, 'result)): t(int => 'func, 'result)
  | Float(t('func, 'result)): t(float => 'func, 'result)
  | Wildcard(t('func, 'result)): t('func, 'result)
  | Custom(string => 'a, t('func, 'result)): t('a => 'func, 'result);

let split: Refract_HttpContext.t => list(string) =
  ctx =>
    List.filter(
      fun
      | "" => false
      | _ => true,
      Refract_CrossplatString.splitOnChar(
        '/',
        List.hd(
          Refract_CrossplatString.splitOnChar(
            '?',
            Refract_Request.url(ctx.request),
          ),
        ),
      ),
    );

let swizzle = (f, a, b) => f(b, a);

let rec evalPath:
  type func result. (func, t(func, result), list(string)) => result =
  (f, route, parts) =>
    switch (route, parts) {
    | (End, []) => f()
    | (_, []) => raise(RouteDoesNotMatch)
    | (End, _) => raise(RouteDoesNotMatch)
    | (Constant(value, tl), [str, ...next]) when value == str =>
      evalPath(f, tl, next)
    | (Constant(_), _) => raise(RouteDoesNotMatch)
    | (String(tl), [str, ...next]) => evalPath(f(str), tl, next)
    | (Int(tl), [str, ...next]) =>
      let value =
        try (int_of_string(str)) {
        | Failure(_) => raise(RouteDoesNotMatch)
        };
      evalPath(f(value), tl, next);
    | (UInt(tl), [str, ...next]) =>
      let value =
        try (int_of_string(str)) {
        | Failure(_) => raise(RouteDoesNotMatch)
        };
      if (value < 0) {
        raise(RouteDoesNotMatch);
      } else {
        evalPath(f(value), tl, next);
      };
    | (Float(tl), [str, ...next]) =>
      let value =
        try (float_of_string(str)) {
        | Failure(_) => raise(RouteDoesNotMatch)
        };
      evalPath(f(value), tl, next);
    | (Wildcard(tl), [_, ...next]) =>
      try (evalPath(f, tl, next)) {
      | RouteDoesNotMatch => evalPath(f, Wildcard(tl), next)
      }
    | (Custom(parser, tl), [str, ...next]) =>
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