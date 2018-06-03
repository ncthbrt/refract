type t = [
  | `Empty
  | `String(string)
  | `Stream((string => Repromise.t(unit)) => Repromise.t(unit))
];