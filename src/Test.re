/*
   Goal:
   Transform:
   [%route "/hello/%s"]
   To:
   (f, ctx) =>
    switch (Route.evaluate(ctx, [| Constant("hello"), String("name") |])) {
    | [String(v0)] => f(~name=v0, ctx)
    | _ => raise(Failure("This case should never be executed"))
    | exception Route.RouteDoesNotMatch => Reconstruct.unhandled(ctx)
    };

    Ast representation of this:


 */
/* let a = (f, ctx) =>
   switch (Route.evaluate(ctx, [Constant("hello"), String("name")])) {
   | [String(v0)] => f(~name=Some(v0), ctx)
   | _ => raise(Failure("This case should never be executed"))
   | exception Route.RouteDoesNotMatch => Reconstruct.unhandled(ctx)
   }; */
/*
   Function application:
 Pexp_apply(
       Pexp_ident("f"),
       [
         /* First param is optional arg, second is value */
         ("name", Pexp_constant(Pexp_ident("v0"))),
         ("", Pexp_constant(Pexp_ident("ctx"))
       ]
  )
  */
/* switch (a) {
   | [1, 2] => true
   | _ => false
   | exception Not_found => false
   }; */
/*
 Switch/Match statement
  [
      Pstr_eval(
        Pexp_match(
          Pexp_ident "a",
          [
                (
                  Ppat_construct (
                    "::",
                    Some(
                      Ppat_tuple(
                      [
                          Ppat_constant(Const_int(1)),
                          Ppat_construct(
                            "::",
                            Some(
                                Ppat_tuple(
                                  [

                                      Ppat_constant(Const_int(2)),
                                      Ppat_construct ("[]", None)
                                  ]
                                )
                            )
                          )
                      ])
                    )),
                  Pexp_construct("true"),
                  None
                ),
                (
                  Ppat_any,
                  Pexp_construct("false"),
                  None
                ),
                (
                  Ppat_exception(
                    Ppat_construct("Not_found"),
                    None
                  ),
                  Pexp_construct ("false"),
                  None
                )
          ]
      )
    )
  ]
   */