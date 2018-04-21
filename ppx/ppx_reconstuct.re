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
    | Pexp_extension((
        {Asttypes.txt: "route", _},
        PStr([
          {
            pstr_desc:
              Pstr_eval(
                {pexp_desc: Pexp_constant(Pconst_string(str, _)), _},
                _,
              ),
            _,
          },
        ]),
      )) =>
      print_endline("hello " ++ str);
      Ast_helper.(Exp.constant(~loc=e.pexp_loc, Const.string(str)));
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