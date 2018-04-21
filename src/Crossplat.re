let uppercaseAsciiChar = c =>
  if (c >= 'a' && c <= 'z') {
    Char.unsafe_chr(Char.code(c) - 32);
  } else {
    c;
  };

let uppercaseAscii = String.map(uppercaseAsciiChar);