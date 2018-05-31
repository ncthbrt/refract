exception RouteDoesNotMatch;

exception MalformedQueryParameter(string, string, exn);

type t('tuple, 'func, 'result) =
  | End: t(unit, 'func, 'result)
  | Constant(string, t('tuple, 'func, 'result)): t('tuple, 'func, 'result)
  | String(string, t('tuple, 'func, 'result)): t(
                                                   (string, 'tuple),
                                                   string => 'func,
                                                   'result,
                                                 )
  | Int(string, t('tuple, 'func, 'result)): t(
                                                (int, 'tuple),
                                                int => 'func,
                                                'result,
                                              )
  | Float(string, t('tuple, 'func, 'result)): t(
                                                  (float, 'tuple),
                                                  float => 'func,
                                                  'result,
                                                )
  | Wildcard(t('tuple, 'func, 'result)): t('tuple, 'func, 'result)
  | Custom(string, string => 'a, t('tuple, 'func, 'result)): t(
                                                                 ('a, 'tuple),
                                                                 'a => 'func,
                                                                 'result,
                                                               );

let rec evalPath:
  type func result tuple. (t(tuple, result, func), list(string)) => tuple =
  (route, parts) =>
    switch (route, parts) {
    | (End, []) => ()
    | (_, []) => raise(RouteDoesNotMatch)
    | (End, _) => raise(RouteDoesNotMatch)
    | (Constant(value, tl), [str, ...next]) when value == str =>
      evalPath(tl, next)
    | (Constant(_), _) => raise(RouteDoesNotMatch)
    | (String(_, tl), [str, ...next]) => (str, evalPath(tl, next))
    | (Int(_, tl), [str, ...next]) =>
      let value =
        try (int_of_string(str)) {
        | Failure(_) => raise(RouteDoesNotMatch)
        };
      (value, evalPath(tl, next));
    | (Float(_, tl), [str, ...next]) =>
      let value =
        try (float_of_string(str)) {
        | Failure(_) => raise(RouteDoesNotMatch)
        };
      (value, evalPath(tl, next));
    | (Wildcard(tl), [_, ...next]) =>
      try (evalPath(tl, next)) {
      | RouteDoesNotMatch => evalPath(Wildcard(tl), next)
      }
    | (Custom(_, parser, tl), [str, ...next]) =>
      let value =
        try (parser(str)) {
        | _ => raise(RouteDoesNotMatch)
        };
      (value, evalPath(tl, next));
    };

let matches:
  type tuple func.
    (t(tuple, Refract_Machine.t, func), func) => Refract_Machine.t =
  (path, f) => Refract_Machine.handled;