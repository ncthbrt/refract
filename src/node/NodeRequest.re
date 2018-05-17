type t;

module Node = {
  module Http2Stream = {
    type t;
  };
  [@bs.get] external methodAsStr : t => string = "method";
  [@bs.get] external url : t => string = "";
  [@bs.get] external httpVersion : t => string = "";
  [@bs.get] external rawHeaders : t => array(string) = "";
  [@bs.get] external stream : t => Http2Stream.t = "";
  [@bs.send]
  external on :
    (
      t,
      [@bs.string] [ | `data(Node.buffer => unit) | `error(unit => unit)]
    ) =>
    unit =
    "";
};

let method_ = req =>
  Method.fromString(Node.methodAsStr(req)) |. Belt.Option.getExn;

let headers = req => {
  let rec aux = (prev, headers) =>
    switch (headers) {
    | [k, v, ...tail] => aux([(k, v), ...prev], tail)
    | [] => prev
    | [k] => [(k, ""), ...prev]
    };
  aux([], Node.rawHeaders(req) |. Belt.List.fromArray);
};

let httpVersion = Node.httpVersion;

let url = Node.url;