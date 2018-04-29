/* Float("hello", true); */
/*
   structure_item (./ppx/asttest.re[1,0+0]..[1,0+20])
     Pstr_eval
     expression (./ppx/asttest.re[1,0+0]..[1,0+20])
       attribute "explicit_arity"
         []
       Pexp_construct "Float" (./ppx/asttest.re[1,0+0]..[1,0+5])
       Some
         expression (./ppx/asttest.re[1,0+5]..[1,0+20])
           Pexp_tuple
           [
             expression (./ppx/asttest.re[1,0+6]..[1,0+13])
               Pexp_constant Const_string("hello",None)
             expression (./ppx/asttest.re[1,0+15]..[1,0+19])
               Pexp_construct "true" (./ppx/asttest.re[1,0+15]..[1,0+19])
               None
           ]
 ] */