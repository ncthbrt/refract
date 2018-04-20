/* [%route "/hello/%s"]("hello"); */
/* Pstr_eval(
        Pexp_apply,
        Pexp_ident("f"),
        [
            Pexp_constant (Const_string("hello",None)),
            Pexp_constant (Const_string("world",None))
        ]
    )
   */
/*

 */
/* let bVariable = [
     Reconstruct.Route.Constant("hello"),
     Reconstruct.Route.String,
   ]; */
/* (f, ctx: Reconstruct.HttpContext.t) =>
   switch (Reconstruct.Route.evaluate(ctx, bVariable)) {
   | [String(v1)] => f(~name=v1, ctx)
   | _ => Reconstruct.Machine.unhandled(ctx)
   | exception Reconstruct.Route.RouteDoesNotMatch =>
     Reconstruct.Machine.unhandled(ctx)
   }; */
(f, eep) => f(~name="hello");
/* [
     structure_item
       Pstr_eval
       expression
         Pexp_fun ""
         None
         pattern
           Ppat_var "f"
         expression
           Pexp_fun ""
           None
           pattern
             Ppat_var "eep"
           expression
             Pexp_apply
             expression (./Test.re[25,635+12]..[25,635+13])
               Pexp_ident "f" (./Test.re[25,635+12]..[25,635+13])
             [
               (<label> "name", Pexp_constant(Const_string("hello",None)),
               (<label> "name", Pexp_constant(Const_string("hello",None))
             ]
   ] */