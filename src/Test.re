/* [%route "/hello/%s"]((~name, ctx) => ()); */
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
 [

     Pstr_eval

       Pexp_match

         Pexp_ident "a"
       [
         <case>
             Ppat_construct "::"
             Some
                 Ppat_tuple
                 [

                     Ppat_constant Const_int 1

                     Ppat_construct "::"
                     Some

                         Ppat_tuple
                         [

                             Ppat_constant Const_int 2
                             Ppat_construct "[]"
                             None
                         ]
                 ]
           expression (./Test.re[14,312+12]..[14,312+16])
             Pexp_construct "true" (./Test.re[14,312+12]..[14,312+16])
             None
         <case>
           pattern (./Test.re[15,329+2]..[15,329+3])
             Ppat_any
           expression (./Test.re[15,329+7]..[15,329+12])
             Pexp_construct "false" (./Test.re[15,329+7]..[15,329+12])
             None
         <case>
           pattern (./Test.re[16,342+2]..[16,342+21])
             Ppat_exception
             pattern (./Test.re[16,342+12]..[16,342+21])
               Ppat_construct "Not_found" (./Test.re[16,342+12]..[16,342+21])
               None
           expression (./Test.re[16,342+25]..[16,342+30])
             Pexp_construct "false" (./Test.re[16,342+25]..[16,342+30])
             None
       ]
 ]
  */