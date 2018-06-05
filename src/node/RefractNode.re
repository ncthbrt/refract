module Request = {
  type t;
  [@bs.get] external methodAsStr : t => string = "method";
  [@bs.get] external url : t => string = "";
  [@bs.get] external httpVersion : t => string = "";
  [@bs.get] external rawHeaders : t => array(string) = "";
  [@bs.send]
  external on :
    (
      t,
      [@bs.string] [
        | `data(Node.buffer => unit)
        | `error(unit => unit)
        | [@bs.as "end"] `end_(unit => unit)
      ]
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
  external write : (t, string, [@bs.string] [ | `utf8], unit => unit) => unit =
    "write";
  [@bs.send]
  external writeHead : (t, int, Js.Dict.t(string)) => unit = "writeHead";
  [@bs.send] external end_ : t => unit = "end";
};

module Buffer = {
  [@bs.val "Buffer.concat"]
  external concat : array(Node.Buffer.t) => Node.Buffer.t = "";
};