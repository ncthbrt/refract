exception RouteDoesNotMatch;

exception MalformedPathString(string);

type path =
  | Constant(string)
  | String(string)
  | Int(string)
  | UInt(string)
  | Float(string)
  | Wildcard;

type isOptional = bool;

type query =
  | FlagQuery(string)
  | BoolQuery(string, isOptional)
  | StringQuery(string, isOptional)
  | IntQuery(string, isOptional)
  | UIntQuery(string, isOptional)
  | FloatQuery(string, isOptional);

type t = (list(path), list(query));

type resultPart =
  | StringResult(string)
  | IntResult(int)
  | FloatResult(float);

type queryResult =
  | StringQueryResult(option(string))
  | IntQueryResult(option(int))
  | FloatQueryResult(option(float))
  | BoolQueryResult(option(bool));

type result = (list(resultPart), list(queryResult));

let validateName =
  fun
  | "" => true
  | name =>
    Crossplat.String.matches(Str.regexp("^[a-z_][0-9a-zA-Z_']*$"), name);

let parse: string => t =
  route => {
    let parseQueryItem = (item: string) => {
      let (item, isOptional) =
        switch (Crossplat.String.split(Str.regexp("\\=\\?"), item)) {
        | [name] => (name, String.length(name) < String.length(item))
        | [item, rest] =>
          raise(
            MalformedPathString(
              "Expected '&' or end of string after '=?' but got " ++ rest,
            ),
          )
        | [a, ...b] =>
          print_endline(a);
          raise(
            MalformedPathString(
              "Too many optional annotations ('=?') in query string",
            ),
          );
        | [] =>
          raise(
            MalformedPathString(
              "Too many optional annotations ('=?') in query string",
            ),
          )
        };
      switch (String.split_on_char(':', item)) {
      | [name, "string"] when validateName(name) =>
        StringQuery(name, isOptional)
      | [name, "float"] when validateName(name) =>
        FloatQuery(name, isOptional)
      | [name, "int"] when validateName(name) => IntQuery(name, isOptional)
      | [name, "uint"] when validateName(name) =>
        UIntQuery(name, isOptional)
      | [name, "bool"] when validateName(name) =>
        BoolQuery(name, isOptional)
      | [name, _] when ! validateName(name) =>
        raise(
          MalformedPathString("Identifier Name " ++ name ++ " not allowed"),
        )
      | [_, type_] =>
        raise(
          MalformedPathString("Type " ++ type_ ++ "is not yet supported"),
        )
      | [name] when isOptional => FlagQuery(name)
      | [name] =>
        raise(
          MalformedPathString(
            "No type annotation on '"
            ++ name
            ++ "' in query params. Either add '=?' to make it a flag, add a type annotation or do both.",
          ),
        )
      | _ => raise(MalformedPathString("Too many type delimiters"))
      };
    };
    let rec parseQuery: list(string) => list(query) =
      fun
      | [] => []
      | [hd, ...tail] => [parseQueryItem(hd), ...parseQuery(tail)];
    let rec parsePath: list(string) => list(path) =
      fun
      | [] => []
      | ["*", ...tail] => [Wildcard, ...parsePath(tail)]
      | [hd, ...tail] =>
        switch (String.split_on_char(':', hd)) {
        | [constant] => [
            Constant(Crossplat.String.lowercaseAscii(constant)),
            ...parsePath(tail),
          ]
        | [name, "int"] when validateName(name) => [
            Int(name),
            ...parsePath(tail),
          ]
        | [name, "uint"] when validateName(name) => [
            UInt(name),
            ...parsePath(tail),
          ]
        | [name, "string"] when validateName(name) => [
            String(name),
            ...parsePath(tail),
          ]
        | [name, "float"] when validateName(name) => [
            Float(name),
            ...parsePath(tail),
          ]
        | [name, _] when ! validateName(name) =>
          raise(
            MalformedPathString("Identifier Name " ++ name ++ " not allowed"),
          )
        | [name, type_] =>
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
      switch (Crossplat.String.splitFirst(~on=Str.regexp("\\?"), route)) {
      | [route, query] => (
          parsePath(Crossplat.String.split(~on=Str.regexp("/+"), route)),
          parseQuery(Crossplat.String.split(~on=Str.regexp("&"), query)),
        )
      | [route] => (
          parsePath(Crossplat.String.split(~on=Str.regexp("/+"), route)),
          [],
        )
      | _ =>
        raise(
          Failure(
            "This case should never occur. It means that there is a logic bug in the route parsing code",
          ),
        )
      };
    };
  };

let evaluate = ({request: {resource}}: HttpContext.t, route: t) => {
  let rec evalQuery = (query, queryParts: list(query)) =>
    raise(RouteDoesNotMatch);
  let rec evalPath: (_, _) => list(resultPart) =
    (path, route: list(path)) =>
      switch (path, route) {
      | ([], []) => []
      | ([_, ..._], []) => raise(RouteDoesNotMatch)
      | ([], _) => raise(RouteDoesNotMatch)
      | ([hd, ...tl], [Constant(str), ...next]) when str == hd =>
        evalPath(tl, next)
      | (_, [Constant(_), ..._]) => raise(RouteDoesNotMatch)
      | ([hd, ...tl], [String(_), ...next]) => [
          StringResult(hd),
          ...evalPath(tl, next),
        ]
      | ([hd, ...tl], [Int(_), ...next]) =>
        try ([IntResult(int_of_string(hd)), ...evalPath(tl, next)]) {
        | Failure(_) => raise(RouteDoesNotMatch)
        }
      | ([hd, ...tl], [UInt(_), ...next]) =>
        let value =
          try (int_of_string(hd)) {
          | Failure(_) => raise(RouteDoesNotMatch)
          };
        value >= 0 ?
          [IntResult(value), ...evalPath(tl, next)] :
          raise(RouteDoesNotMatch);
      | ([hd, ...tl], [Float(_), ...next]) =>
        try ([FloatResult(float_of_string(hd)), ...evalPath(tl, next)]) {
        | Failure(_) => raise(RouteDoesNotMatch)
        }
      | ([_, ...tl], [Wildcard, ...next]) =>
        try (evalPath(tl, next)) {
        | RouteDoesNotMatch => evalPath(tl, [Wildcard, ...next])
        }
      };
  let (path, query) =
    switch (String.split_on_char('?', resource)) {
    | [path, query] => (
        Crossplat.String.split(Str.regexp("/+"), path),
        Crossplat.String.split(Str.regexp("&+"), query),
      )
    | [path] => (Crossplat.String.split(Str.regexp("/+"), path), [])
    | _ => raise(Failure("This should never have been called"))
    };
  (evalPath(path, fst(route)), evalQuery(query, snd(route)));
};