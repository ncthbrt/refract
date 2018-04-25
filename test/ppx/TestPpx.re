let () = {
  let unlabelledMachine =
    [%route "/hello/:string"](name => {
      print_endline("hello unlabelled " ++ name);
      Reconstruct.Machine.handled;
    });
  let labelledMachine =
    [%route.get "/hello/name:string"]((~name) => {
      print_endline("hello labelled " ++ name);
      Reconstruct.Machine.handled;
    });
  ();
};