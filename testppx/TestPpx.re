let () = {
  let a: string = [%route "goodbye"];
  print_endline(a ++ " world");
};