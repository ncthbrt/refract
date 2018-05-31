type t = Refract_Node.Request.t;

let method_ = req =>
  Refract_Method.fromString(Refract_Node.Request.methodAsStr(req))
  |. Belt.Option.getExn;

let headers = req => {
  let rec aux = (prev, headers) =>
    switch (headers) {
    | [k, v, ...tail] => aux([(k, v), ...prev], tail)
    | [] => prev
    | [k] => [(k, ""), ...prev]
    };
  aux([], Refract_Node.Request.rawHeaders(req) |. Belt.List.fromArray);
};

let httpVersion = req => Refract_Node.Request.httpVersion(req);

let url = req => Refract_Node.Request.url(req);