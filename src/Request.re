module Method = {
  type t =
    /* Has no request body */
    | Get
    /* Has no request body and no response body */
    | Head
    | Post
    /* Has no response body and is idempotent */
    | Put
    /* May have both req/res body and is idempotent */
    | Delete
    /* No req body and has res body */
    | Connect
    /* No req body and has res body, safe, idempotent */
    | Options
    /* No req body and no res body */
    | Trace
    /* Has req body and no res body */
    | Patch;
  let fromString = str =>
    switch (Crossplat.String.uppercaseAscii(str)) {
    | "GET" => Some(Get)
    | "HEAD" => Some(Head)
    | "POST" => Some(Post)
    | "PUT" => Some(Put)
    | "DELETE" => Some(Delete)
    | "CONNECT" => Some(Connect)
    | "OPTIONS" => Some(Options)
    | "TRACE" => Some(Trace)
    | "PATCH" => Some(Patch)
    | _ => None
    };
  let toString =
    fun
    | Get => "GET"
    | Head => "HEAD"
    | Post => "POST"
    | Put => "PUT"
    | Delete => "DELETE"
    | Connect => "CONNECT"
    | Options => "OPTIONS"
    | Trace => "TRACE"
    | Patch => "PATCH";
};

module Body = {
  type t;
};

type t = {
  path: string,
  headers: list((string, option(string))),
  method: Method.t,
  body: Body.t,
};

let headers = ({headers}) => headers;

let header = ({headers}, header) =>
  try (snd(List.find(((k, _)) => k == header, headers))) {
  | Not_found => None
  };