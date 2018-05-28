type t = Reconstruct_Node.Request.t;

let method_ = req =>
  Reconstruct_Method.fromString(Reconstruct_Node.Request.methodAsStr(req))
  |. Belt.Option.getExn;

let headers = req => {
  let rec aux = (prev, headers) =>
    switch (headers) {
    | [k, v, ...tail] => aux([(k, v), ...prev], tail)
    | [] => prev
    | [k] => [(k, ""), ...prev]
    };
  aux([], Reconstruct_Node.Request.rawHeaders(req) |. Belt.List.fromArray);
};

let httpVersion = req => Reconstruct_Node.Request.httpVersion(req);

let url = req => Reconstruct_Node.Request.url(req);