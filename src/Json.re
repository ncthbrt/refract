type t = [
  | `Null
  | `Bool(bool)
  | `Float(float)
  | `String(string)
  | `Assoc(list((string, t)))
  | `List(list(t))
];
