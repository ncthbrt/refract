/*
 Source code:
     let f = () => {
       %refract.await
       Refract.Response.ok;
       Refract.Response.Body.string("ok");
     };
  */
/* Source AST
      [
     structure_item (./src/ppx/Test.re[1,0+0]..[5,94+1])
       Pstr_value Nonrec
       [
         <def>
           pattern (./src/ppx/Test.re[1,0+4]..[1,0+5])
             Ppat_var "f" (./src/ppx/Test.re[1,0+4]..[1,0+5])
           expression (./src/ppx/Test.re[1,0+8]..[5,94+1])
             Pexp_fun ""
             None
             pattern (./src/ppx/Test.re[1,0+8]..[1,0+10])
               Ppat_construct "()" (./src/ppx/Test.re[1,0+8]..[1,0+10])
               None
             expression (./src/ppx/Test.re[1,0+14]..[5,94+1])
               Pexp_sequence
               expression ([0,0+-1]..[0,0+-1]) ghost
                 Pexp_extension "refract.await"
                 [
                   structure_item (./src/ppx/Test.re[3,33+2]..[3,33+21])
                     Pstr_eval
                     expression (./src/ppx/Test.re[3,33+2]..[3,33+21])
                       Pexp_ident "Refract.Response.ok" (./src/ppx/Test.re[3,33+2]..[3,33+21])
                 ]
               expression (./src/ppx/Test.re[4,56+2]..[4,56+37])
                 Pexp_apply
                 expression (./src/ppx/Test.re[4,56+2]..[4,56+30])
                   Pexp_ident "Refract.Response.Body.string" (./src/ppx/Test.re[4,56+2]..[4,56+30])
                 [
                   <label> ""
                     expression (./src/ppx/Test.re[4,56+31]..[4,56+35])
                       Pexp_constant Const_string("ok",None)
                 ]
       ]
   ]
      */
/* Target Code: let f = () => Refract.compose(Refract.Response.ok, Refract.Response.Body.string("ok")); */
/* Target AST
      [
     structure_item (./src/ppx/Test.re[46,1732+0]..[47,1746+74])
       Pstr_value Nonrec
       [
         <def>
           pattern (./src/ppx/Test.re[46,1732+4]..[46,1732+5])
             Ppat_var "f" (./src/ppx/Test.re[46,1732+4]..[46,1732+5])
           expression (./src/ppx/Test.re[46,1732+8]..[47,1746+74])
             Pexp_fun ""
             None
             pattern (./src/ppx/Test.re[46,1732+8]..[46,1732+10])
               Ppat_construct "()" (./src/ppx/Test.re[46,1732+8]..[46,1732+10])
               None
             expression (./src/ppx/Test.re[47,1746+2]..[47,1746+74])
               Pexp_apply
               expression (./src/ppx/Test.re[47,1746+2]..[47,1746+17])
                 Pexp_ident "Refract.compose" (./src/ppx/Test.re[47,1746+2]..[47,1746+17])
               [
                 <label> ""
                   expression (./src/ppx/Test.re[47,1746+18]..[47,1746+37])
                     Pexp_ident "Refract.Response.ok" (./src/ppx/Test.re[47,1746+18]..[47,1746+37])
                 <label> ""
                   expression (./src/ppx/Test.re[47,1746+39]..[47,1746+73])
                     Pexp_apply
                     expression (./src/ppx/Test.re[47,1746+39]..[47,1746+67])
                       Pexp_ident "Refract.Response.Body.string" (./src/ppx/Test.re[47,1746+39]..[47,1746+67])
                     [
                       <label> ""
                         expression (./src/ppx/Test.re[47,1746+68]..[47,1746+72])
                           Pexp_constant Const_string("ok",None)
                     ]
               ]
       ]
   ]
       */