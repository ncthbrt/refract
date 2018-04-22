let () = {
  let a = [%route "/what/name:string"];
  let p =
    Repromise.then_(
      result => Repromise.resolve(),
      a(
        (str, ctx) => {
          print_endline("hello" ++ str);
          Reconstruct.Machine.handled(ctx);
        },
        {
          request: {
            path: "/what/frog",
            headers: [],
            method: Get,
            body: Obj.magic("world"),
          },
          response: Obj.magic("world"),
        },
      ),
    );
  ();
  /* Array.iteri(
       (v, i) =>
         print_int(
           v,
           /* print_newline(); */
         ),
       Array.make(100, 1),
     ); */
  /* print_endline(a("hello", Obj.magic("world")) ++ " world"); */
};