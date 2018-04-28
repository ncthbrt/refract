open Reconstruct.Operators;

let () = {
  let unlabelledMachine: Reconstruct.Machine.t =
    [%route "/hello/:string"](name => {
      print_endline("hello unlabelled " ++ name);
      Reconstruct.Machine.handled;
    });
  let labelledMachine: Reconstruct.Machine.t =
    [%route.get "/hello/name:string/surname:string"]((~name, ~surname) =>
      Reconstruct.Request.bodyText(body => {
        print_endline("hello labelled " ++ name ++ " " ++ surname);
        Reconstruct.Machine.handled;
      })
    );
  let labelledMachine2: Reconstruct.Machine.t =
    [%route.post "/hello/name:string"]((~name, ctx) => {
      print_endline("hello labelled " ++ name);
      Reconstruct.Machine.handled(ctx);
    });
  ();
};