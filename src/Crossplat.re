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
  let split = (~on, str) => Str.split(on, str);
};