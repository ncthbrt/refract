let () = {
  let unlabelledMachine: Reconstruct.Machine.t =
    [%route "/hello/:string"](name => {
      print_endline("hello unlabelled " ++ name);
      Reconstruct.Machine.handled;
    });
  let labelledMachine: Reconstruct.Machine.t =
    [%route.get "/hello/name:string"]((~name) => {
      print_endline("hello labelled " ++ name);
      Reconstruct.Machine.handled;
    });
  ();
};