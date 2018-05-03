module String = {
  let uppercaseAsciiChar = c =>
    if (c >= 'a' && c <= 'z') {
      Char.unsafe_chr(Char.code(c) - 32);
    } else {
      c;
    };
  let lowercaseAsciiChar = c =>
    if (c >= 'A' && c <= 'Z') {
      Char.unsafe_chr(Char.code(c) + 32);
    } else {
      c;
    };
  let uppercaseAscii = String.map(uppercaseAsciiChar);
  let lowercaseAscii = String.map(lowercaseAsciiChar);
  let split = (~on, str) => Str.split(Str.regexp(on), str);
  let splitOnChar = (~on, str) => String.split_on_char(on, str);
  let splitFirst = (~on, str) => Str.bounded_split(Str.regexp(on), str, 2);
  let matches = (~regex, str) =>
    Str.string_match(Str.regexp(regex), str, 0);
};