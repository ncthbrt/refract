open Reconstruct.Operators;

let () =
  ignore(
    Reconstruct.switch_(
      [
        [%route "/hello1/:string"](name => {
          print_endline("hello unlabelled " ++ name);
          Reconstruct.Machine.handled;
        }),
        [%route.get "/hello2/name:string/surname:string"]((~name, ~surname) =>
          Reconstruct.Request.bodyText(body => {
            print_endline("hello labelled " ++ name ++ " " ++ surname);
            Reconstruct.Machine.handled;
          })
        ),
        [%route.post "/hello3/name:string"]((~name) => {
          let%mesh body = Reconstruct.Request.bodyText;
          print_endline("hello labelled " ++ name ++ " " ++ body);
          Reconstruct.Machine.handled;
        }),
        [%route.post "/hello4/name:string?offset:int=?&limit:uint=?"](
          (~name, ~offset=0, ~limit=10) => {
          let%mesh body = Reconstruct.Request.bodyText;
          print_endline("hello labelled " ++ name ++ " " ++ body);
          Reconstruct.Machine.handled;
        }),
      ],
      Obj.magic(),
    ),
  );