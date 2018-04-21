exception RouteDoesNotMatch;

exception MalformedPathString(string);

type part =
  | Constant(string)
  | String(string)
  | Int(string)
  | UInt(string)
  | Float(string)
  | Wildcard;

type t = list(part);

type evalPart =
  | String(string)
  | Int(int)
  | Float(float);

type evaluatedRoute = list(evalPart);

let parse = route => {
  let rec aux: list(string) => t =
    fun
    | [] => []
    | ["*", ...tail] => [Wildcard, ...aux(tail)]
    | [hd, ...tail] =>
      switch (String.split_on_char(':', hd)) {
      | [constant] => [Constant(constant), ...aux(tail)]
      | [name, "int"] => [Int(name), ...aux(tail)]
      | [name, "uint"] => [UInt(name), ...aux(tail)]
      | [name, "string"] => [String(name), ...aux(tail)]
      | [name, "float"] => [Float(name), ...aux(tail)]
      | _ => raise(MalformedPathString("Too many type delimiters"))
      };
  aux(Str.split(Str.regexp("/+"), route));
};

let evaluate = ({request: {path}}: HttpContext.t, route) => {
  let rec aux = (path, route: t) =>
    switch (path, route) {
    | ([], []) => []
    | ([_, ..._], []) => raise(RouteDoesNotMatch)
    | ([], _) => raise(RouteDoesNotMatch)
    | ([hd, ...tl], [Constant(str), ...next]) when str == hd =>
      aux(tl, next)
    | (_, [Constant(_), ..._]) => raise(RouteDoesNotMatch)
    | ([hd, ...tl], [String(_), ...next]) => [
        String(hd),
        ...aux(tl, next),
      ]
    | ([hd, ...tl], [Int(_), ...next]) =>
      try ([Int(int_of_string(hd)), ...aux(tl, next)]) {
      | Failure(_) => raise(RouteDoesNotMatch)
      }
    | ([hd, ...tl], [UInt(_), ...next]) =>
      let value =
        try (int_of_string(hd)) {
        | Failure(_) => raise(RouteDoesNotMatch)
        };
      value >= 0 ?
        [Int(value), ...aux(tl, next)] : raise(RouteDoesNotMatch);
    | ([hd, ...tl], [Float(_), ...next]) =>
      try ([Float(float_of_string(hd)), ...aux(tl, next)]) {
      | Failure(_) => raise(RouteDoesNotMatch)
      }
    | ([_, ...tl], [Wildcard, ...next]) =>
      try (aux(tl, next)) {
      | RouteDoesNotMatch => aux(tl, [Wildcard, ...next])
      }
    };
  aux(Str.split(Str.regexp("/+"), path), route);
};