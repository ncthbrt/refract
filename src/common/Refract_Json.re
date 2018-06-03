type t = [
  | `Null
  | `Bool(bool)
  | `Float(float)
  | `String(string)
  | `Assoc(list((string, t)))
  | `List(list(t))
];

module Decoder = {
  exception DecoderError(string, t, option(exn));
  type nonrec t('a) = t => 'a;
  let bool: t(bool) =
    fun
    | `Bool(value) => value
    | value =>
      raise(DecoderError("Expected value to be a bool", value, None));
  let string: t(string) =
    fun
    | `String(value) => value
    | value =>
      raise(DecoderError("Expected value to be a string", value, None));
  let int: t(int) =
    fun
    | `Float(value) => int_of_float(value)
    | value =>
      raise(DecoderError("Expected value to be an integer", value, None));
  let float: t(float) =
    fun
    | `Float(value) => value
    | value =>
      raise(DecoderError("Expected value to be a float", value, None));
  let list: t('a) => t(list('a)) =
    decoder =>
      fun
      | `List(value) =>
        try (List.map(decoder, value)) {
        | DecoderError(_, _, _) as e =>
          raise(
            DecoderError(
              "Encountered error while attempting to decode list",
              `List(value),
              Some(e),
            ),
          )
        }
      | value =>
        raise(DecoderError("Expected value to be a list", value, None));
  let array: t('a) => t(array('a)) =
    (decoder, value) => Array.of_list(list(decoder, value));
  let char: t(char) =
    json => {
      let s = string(json);
      if (String.length(s) == 1) {
        s.[0];
      } else {
        raise(
          DecoderError(
            "Expected single-character string, got " ++ s,
            json,
            None,
          ),
        );
      };
    };
};

module Encoder = {
  type nonrec t('a) = 'a => t;
};