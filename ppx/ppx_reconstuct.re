module Ast = Ast_406;

module Ast_helper = Ast.Ast_helper;

module Ast_mapper = Ast.Ast_mapper;

module Asttypes = Ast.Asttypes;

module Location = Ast.Location;

module Longident = Ast.Longident;

module Parsetree = Ast.Parsetree;

open Ast_mapper;

open Parsetree;

let mapper = {
  ...default_mapper,
  expr: (mapper, e) =>
    switch (e.pexp_desc) {
    | [@implicit_arity]
      Pexp_extension(
        {Asttypes.txt: "route", _},
        PStr([
          {
            pstr_desc:
              Pstr_value(
                Asttypes.Nonrecursive,
                [
                  {
                    pvb_expr: {
                      pexp_desc: Pexp_constant(Pconst_string(str, _)),
                      _,
                    },
                    _,
                  },
                ],
              ),
            _,
          },
        ]),
      ) =>
      open Ast_helper.Exp;
      /* let evalRoute =
         ident(
           Location.mknoloc(Longident.parse("Reconstruct.Route.evaluate")),
         ); */
      let strRet = constant(Pconst_string(str, None));
      strRet;
    | _ => default_mapper.expr(mapper, e)
    },
};

let () =
  Migrate_parsetree.(
    Driver.register(
      ~name="ppx_reconstruct", Versions.ocaml_406, (_config, _cookies) =>
      mapper
    )
  );