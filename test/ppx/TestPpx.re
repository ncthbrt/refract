open Reconstruct.Operators;

let () = {
  ignore(
    Reconstruct.switch_(
      [
        [%route "/hello1/:string"](name => {
          print_endline("hello unlabelled " ++ name);
          Reconstruct.Machine.handled;
        }),
        [%route "/hello2/name:string/surname:string"]((~name, ~surname) => {
          print_endline("THINGS");
          Reconstruct.Request.bodyString(body => {
            print_endline("hello labelled " ++ name ++ " " ++ surname);
            Reconstruct.Machine.handled;
          });
        }),
        [%route.post "/hello3/name:string"]((~name) => {
          let%mesh body = Reconstruct.Request.bodyString;
          print_endline("hello labelled " ++ name ++ " " ++ body);
          Reconstruct.Machine.handled;
        }),
        [%route.post "/hello4/name:string?offset:int=?&limit:uint=?&cached=?"](
          (~name, ~offset=0, ~limit=10, ~cached) => {
          let%mesh body = Reconstruct.Request.bodyString;
          print_endline(
            "hello labelled "
            ++ name
            ++ " "
            ++ body
            ++ " "
            ++ string_of_int(limit)
            ++ "cached: "
            ++ string_of_bool(cached),
          );
          Reconstruct.Machine.handled;
        }),
      ],
      {
        request: {
          resource: "/hello4/nick?limit=4",
          headers: [],
          method: Post,
          body: Obj.magic(),
        },
        response: {
          status: NotFound,
          headers: [],
        },
      },
    ),
  );
  Io.run();
};