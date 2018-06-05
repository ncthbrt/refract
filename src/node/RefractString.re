let uppercaseAsciiChar = c =>
  if (c >= 'a' && c <= 'z') {
    Char.unsafe_chr(Char.code(c) - 32);
  } else {
    c;
  };

let uppercaseAscii = String.map(uppercaseAsciiChar);

let splitOnChar = (chr, str) =>
  Js.String.split(String.make(1, chr), str) |. Belt.List.fromArray;