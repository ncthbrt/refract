exception RouteDoesNotMatch;

exception MalformedRouteString(string);

exception MalformedQueryString(string);

exception MalformedQueryParameter(string, string, exn);

let validateName =
  fun
  | "" => true
  | name =>
    Reconstruct_CrossplatString.matches(
      ~regex="^[a-z_][0-9a-zA-Z_']*$",
      name,
    );

type pathPart('a) =
  | ConstantPart(string)
  | StringPart(string)
  | IntPart(string)
  | UIntPart(string)
  | FloatPart(string)
  | CustomPart(string, string => 'a)
  | WildcardPart;

type path('ty, 'v) =
  | End: path('v, 'v)
  | Constant(string, path('ty, 'v)): path('ty, 'v)
  | String(string, path('ty, 'v)): path(string => 'ty, 'v)
  | Int(string, path('ty, 'v)): path(int => 'ty, 'v)
  | Float(string, path('ty, 'v)): path(float => 'ty, 'v)
  | Wildcard(path('ty, 'v)): path('ty, 'v)
  | Custom(string, string => 'a, path('ty, 'v)): path('a => 'ty, 'v);

type query('ty, 'v) =
  | End: query('v, 'v)
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

let rec evalPath: type t tv. (list(string), path(t, tv), tv => 'a) => unit =
  (parts, route, f) =>
    switch (route, parts) {
    | (End, []) => []
    | (_, []) => raise(RouteDoesNotMatch)
    | (End, _) => raise(RouteDoesNotMatch)
    | (Constant(str, tl), [hd, ...next]) when str == hd =>
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
/*



 exception MalformedPathString(string);





 type path =
   | Constant(string)
   | String(string)
   | Int(string)
   | UInt(string)
   | Float(string)
   | Wildcard;

 type isOptional = bool;



 type t = (list(path), list(query));

 type resultPart =
   | StringResult(string)
   | IntResult(int)
   | FloatResult(float);

 type queryResultPart =
   | StringQueryResult(option(string))
   | IntQueryResult(option(int))
   | FloatQueryResult(option(float))
   | BoolQueryResult(option(bool));

 type result = (list(resultPart), list(queryResultPart));


 let parse: string => t =
   route => {
     let parseQueryItem = (item: string) => {
       let (item, isOptional) =
         switch (Reconstruct_CrossplatString.split(~on="\\=\\?", item)) {
         | [name] => (name, String.length(name) < String.length(item))
         | [_, rest] =>
           raise(
             MalformedRouteString(
               "Expected '&' or end of string after '=?' but got " ++ rest,
             ),
           )
         | _ =>
           raise(
             MalformedRouteString(
               "Too many optional annotations ('=?') in query string",
             ),
           )
         };
       switch (Reconstruct_CrossplatString.splitOnChar(':', item)) {
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
           MalformedRouteString("Identifier Name " ++ name ++ " not allowed"),
         )
       | [_, type_] =>
         raise(
           MalformedRouteString("Type " ++ type_ ++ "is not yet supported"),
         )
       | [name] when isOptional => FlagQuery(name)
       | [name] =>
         raise(
           MalformedRouteString(
             "No type annotation on '"
             ++ name
             ++ "' in query params. Either add '=?' to make it a flag, add a type annotation or do both.",
           ),
         )
       | _ => raise(MalformedRouteString("Too many type delimiters"))
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
         switch (Reconstruct_CrossplatString.splitOnChar(':', hd)) {
         | [constant] => [
             Constant(Reconstruct_CrossplatString.lowercaseAscii(constant)),
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
             MalformedRouteString(
               "Identifier Name " ++ name ++ " not allowed",
             ),
           )
         | [_, type_] =>
           raise(
             MalformedRouteString("Type " ++ type_ ++ "is not yet supported"),
           )
         | _ => raise(MalformedRouteString("Too many type delimiters"))
         };
     let startsWithSlash =
       try (String.index(route, '/') == 0) {
       | Not_found => false
       };
     if (! startsWithSlash) {
       raise(MalformedRouteString("A route should always begin with a '/'"));
     } else {
       switch (Reconstruct_CrossplatString.splitFirst(~on="\\?", route)) {
       | [route, query] => (
           parsePath(Reconstruct_CrossplatString.split(~on="/+", route)),
           parseQuery(Reconstruct_CrossplatString.split(~on="&", query)),
         )
       | [route] => (
           parsePath(Reconstruct_CrossplatString.split(~on="/+", route)),
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

 let evaluate = ({request: {resource}}: Reconstruct_HttpContext.t, route: t) => {
   let splitQuery = (item: string) =>
     switch (Reconstruct_CrossplatString.splitFirst(~on="\\=", item)) {
     | [key] => (key, None)
     | [key, value] => (key, Some(value))
     | _ =>
       raise(
         Failure(
           "This should never be called. It means that splitFirst has a logic bug which has led it to have be either [] or have more than two values",
         ),
       )
     };
   let findQueryItem = (key, query) =>
     try (Some(List.find(((k, _)) => key == k, query))) {
     | Not_found => None
     };
   let findAndConvertQueryItem = (key, query, converter, isOptional) =>
     switch (findQueryItem(key, query)) {
     | Some((key, Some(value))) =>
       try (Some(converter(value))) {
       | e => raise(MalformedQueryParameter(key, value, e))
       }
     | Some((_, None)) => isOptional ? None : raise(RouteDoesNotMatch)
     | None => isOptional ? None : raise(RouteDoesNotMatch)
     };
   let evalQuery =
       (query: list((string, option(string))), queryParts: list(query)) =>
     List.map(
       fun
       | FlagQuery(name) =>
         switch (findQueryItem(name, query)) {
         | Some((_, Some(value))) =>
           try (
             BoolQueryResult(
               Some(value == "" ? true : bool_of_string(value)),
             )
           ) {
           | e => raise(MalformedQueryParameter(name, "", e))
           }
         | Some((_, None)) => BoolQueryResult(Some(true))
         }
       | BoolQuery(name, isOptional) =>
         BoolQueryResult(
           findAndConvertQueryItem(name, query, bool_of_string, isOptional),
         )
       | StringQuery(name, isOptional) =>
         StringQueryResult(
           findAndConvertQueryItem(name, query, x => x, isOptional),
         )
       | IntQuery(name, isOptional) =>
         IntQueryResult(
           findAndConvertQueryItem(
             name,
             query,
             item => int_of_string(item),
             isOptional,
           ),
         )
       | UIntQuery(name, isOptional) =>
         IntQueryResult(
           findAndConvertQueryItem(
             name,
             query,
             item => {
               let value = int_of_string(item);
               if (value >= 0) {
                 value;
               } else {
                 raise(
                   MalformedQueryParameter(
                     name,
                     item,
                     Failure(
                       name
                       ++ " is required to be a positive integer but received "
                       ++ item
                       ++ " instead",
                     ),
                   ),
                 );
               };
             },
             isOptional,
           ),
         )
       | FloatQuery(name, isOptional) =>
         FloatQueryResult(
           findAndConvertQueryItem(
             name,
             query,
             item => float_of_string(item),
             isOptional,
           ),
         ),
       queryParts,
     );
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
     switch (Reconstruct_CrossplatString.splitOnChar('?', resource)) {
     | [path, query] => (
         Reconstruct_CrossplatString.split(~on="/+", path),
         List.map(
           splitQuery,
           Reconstruct_CrossplatString.split(~on="&+", query),
         ),
       )
     | [path] => (Reconstruct_CrossplatString.split(~on="/+", path), [])
     | _ => raise(Failure("This should never have been called"))
     };
   (evalPath(path, fst(route)), evalQuery(query, snd(route)));
 }; */