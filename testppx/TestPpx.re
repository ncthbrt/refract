let () = {
  let unlabelledNameRoute = [%route "/hello/:string"];
  let labelledNameRoute = [%route "/hello/name:string"];
  let ctx =
    Reconstruct.HttpContext.{
      request: {
        path: "/hello/alfred",
        headers: [],
        method: Get,
        body: Obj.magic("world"),
      },
      response: Obj.magic("world"),
    };
  ignore(
    unlabelledNameRoute(
      (name, ctx) => {
        print_endline("hello unlabelled " ++ name);
        Reconstruct.Machine.handled(ctx);
      },
      ctx,
    ),
  );
  ignore(
    labelledNameRoute(
      (~name, ctx) => {
        print_endline("hello labelled " ++ name);
        Reconstruct.Machine.handled(ctx);
      },
      ctx,
    ),
  );
};