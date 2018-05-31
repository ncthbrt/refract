exception RouteDoesNotMatch;

exception MalformedRouteString(string);

exception MalformedQueryString(string);

exception MalformedQueryParameter(string, string, exn);

type query('ty, 'v) =
  | EndQuery: query('v, 'v)
  | FlagQuery(string, query('ty, 'v)): query(bool => 'ty, 'v)
  | BoolQuery(string, query('ty, 'v)): query(bool => 'ty, 'v)
  | OptionalBoolQuery(string, query('ty, 'v)): query(
                                                   option(bool) => 'ty,
                                                   'v,
                                                 )
  | StringQuery(string, query('ty, 'v)): query(string => 'ty, 'v)
  | OptionalStringQuery(string, query('ty, 'v)): query(
                                                     option(string) => 'ty,
                                                     'v,
                                                   )
  | IntQuery(string, query('ty, 'v)): query(int => 'ty, 'v)
  | OptionalIntQuery(string, query('ty, 'v)): query(option(int) => 'ty, 'v)
  | FloatQuery(string, query('ty, 'v)): query(float => 'ty, 'v)
  | OptionalFloatQuery(string, query('ty, 'v)): query(
                                                    option(float) => 'ty,
                                                    'v,
                                                  )
  | CustomQuery(string, string => 'a, query('ty, 'v)): query('a => 'ty, 'v)
  | OptionalCustomQuery(string, string => 'a, query('ty, 'v)): query(
                                                                   option(
                                                                    float,
                                                                   ) =>
                                                                   'ty,
                                                                   'v,
                                                                 );

type path('ty) =
  | End: path(unit)
  | Constant(string, path('ty)): path('ty)
  | String(string, path('ty)): path((string, 'ty))
  | Int(string, path('ty)): path((int, 'ty))
  | Float(string, path('ty)): path((float, 'ty))
  | Wildcard(path('ty)): path('ty)
  | Custom(string, string => 'a, path('ty)): path(('a, 'ty));

let rec evalPath: type t. (path(t), list(string)) => t =
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