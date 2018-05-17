type t;

module Node = {
  [@bs.send] external setHeader : (t, string, string) => unit = "";
  [@bs.set] external statusCode : (t, int) => unit = "";
  [@bs.set] external statusMessage : (t, string) => unit = "";
  [@bs.send]
  external write : (t, bytes, [@bs.string] [ | `utf8], unit => unit) => unit =
    "write";
  [@bs.send] external end_ : t => unit = "end";
};