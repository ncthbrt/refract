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

type resultPart =
  | StringResult(string)
  | IntResult(int)
  | FloatResult(float);

type result = list(resultPart);

let validateName =
  fun
  | "" => true
  | name => Str.string_match(Str.regexp("^[a-z_][0-9a-zA-Z_']*$"), name, 0);

let parse = route => {
  let rec aux: list(string) => t =
    fun
    | [] => []
    | ["*", ...tail] => [Wildcard, ...aux(tail)]
    | [hd, ...tail] =>
      switch (String.split_on_char(':', hd)) {
      | [constant] => [Constant(constant), ...aux(tail)]
      | [name, "int"] when validateName(name) => [Int(name), ...aux(tail)]
      | [name, "uint"] when validateName(name) => [
          UInt(name),
          ...aux(tail),
        ]
      | [name, "string"] when validateName(name) => [
          String(name),
          ...aux(tail),
        ]
      | [name, "float"] when validateName(name) => [
          Float(name),
          ...aux(tail),
        ]
      | [name, _] when ! validateName(name) =>
        raise(
          MalformedPathString("Identifier Name " ++ name ++ " not allowed"),
        )
      | [name, type_] when ! validateName(name) =>
        raise(
          MalformedPathString("Type " ++ type_ ++ "is not yet supported"),
        )
      | _ => raise(MalformedPathString("Too many type delimiters"))
      };
  let startsWithSlash =
    try (String.index(route, '/') == 0) {
    | Not_found => false
    };
  if (! startsWithSlash) {
    raise(MalformedPathString("A route should always begin with a '/'"));
  } else {
    aux(
      Crossplat.String.split(
        ~on=Str.regexp("/+"),
        Crossplat.String.lowercaseAscii(route),
      ),
    );
  };
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
        StringResult(hd),
        ...aux(tl, next),
      ]
    | ([hd, ...tl], [Int(_), ...next]) =>
      try ([IntResult(int_of_string(hd)), ...aux(tl, next)]) {
      | Failure(_) => raise(RouteDoesNotMatch)
      }
    | ([hd, ...tl], [UInt(_), ...next]) =>
      let value =
        try (int_of_string(hd)) {
        | Failure(_) => raise(RouteDoesNotMatch)
        };
      value >= 0 ?
        [IntResult(value), ...aux(tl, next)] : raise(RouteDoesNotMatch);
    | ([hd, ...tl], [Float(_), ...next]) =>
      try ([FloatResult(float_of_string(hd)), ...aux(tl, next)]) {
      | Failure(_) => raise(RouteDoesNotMatch)
      }
    | ([_, ...tl], [Wildcard, ...next]) =>
      try (aux(tl, next)) {
      | RouteDoesNotMatch => aux(tl, [Wildcard, ...next])
      }
    };
  aux(Str.split(Str.regexp("/+"), path), route);
};