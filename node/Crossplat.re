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
  let split = (~on, str) =>
    Js.String.splitByRe(Js.Re.fromString(on), str) |. Belt.List.fromArray;
  let splitOnChar = (chr, str) =>
    Js.String.split(String.make(1, chr), str) |. Belt.List.fromArray;
  let splitFirst = (~on, str) => {
    let regexp = Js.Re.fromString(on);
    switch (Js.Re.exec(str, regexp)) {
    | Some(result) =>
      let index = Js.Re.index(result);
      [
        Js.String.substrAtMost(~from=0, ~length=index, str),
        Js.String.substringToEnd(~from=index, str),
      ];
    | None => []
    };
  };
  let matches = (~regex, str) => Js.Re.test(str, Js.Re.fromString(regex));
};