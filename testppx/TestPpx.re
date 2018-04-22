let () = {
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
    [%route "/hello/:string"](
      name => {
        print_endline("hello unlabelled " ++ name);
        Reconstruct.Machine.handled;
      },
      ctx,
    ),
  );
  ignore(
    [%route "/hello/name:string"](
      (~name) => {
        print_endline("hello labelled " ++ name);
        Reconstruct.Machine.handled;
      },
      ctx,
    ),
  );
};