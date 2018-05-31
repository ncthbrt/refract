module Http2Stream = {
  type t;
};

module Request = {
  type t;
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

module Response = {
  type t;
  [@bs.send] external setHeader : (t, string, string) => unit = "";
  [@bs.set] external statusCode : (t, int) => unit = "";
  [@bs.set] external statusMessage : (t, string) => unit = "";
  [@bs.send]
  external write : (t, bytes, [@bs.string] [ | `utf8], unit => unit) => unit =
    "write";
  [@bs.send] external end_ : t => unit = "end";
};