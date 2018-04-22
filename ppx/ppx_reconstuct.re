module Ast = Ast_406;

module Ast_helper = Ast.Ast_helper;

module Ast_mapper = Ast.Ast_mapper;

module Asttypes = Ast.Asttypes;

module Location = Ast.Location;

module Longident = Ast.Longident;

module Parsetree = Ast.Parsetree;

open Ast_mapper;

open Parsetree;

module PathParts = {
  let floatP = name => {
    open Ast_helper;
    let loc = Location.mknoloc(Longident.parse("Reconstruct.Route.Float"));
    Exp.construct(
      loc,
      Some(Ast_helper.Exp.constant(Ast_helper.Const.string(name))),
    );
  };
  let string = name => {
    open Ast_helper;
    let loc = Location.mknoloc(Longident.parse("Reconstruct.Route.String"));
    Exp.construct(
      loc,
      Some(Ast_helper.Exp.constant(Ast_helper.Const.string(name))),
    );
  };
  let int = name => {
    open Ast_helper;
    let loc = Location.mknoloc(Longident.parse("Reconstruct.Route.Int"));
    Exp.construct(
      loc,
      Some(Ast_helper.Exp.constant(Ast_helper.Const.string(name))),
    );
  };
  let uint = name => {
    open Ast_helper;
    let loc = Location.mknoloc(Longident.parse("Reconstruct.Route.UInt"));
    Exp.construct(
      loc,
      Some(Ast_helper.Exp.constant(Ast_helper.Const.string(name))),
    );
  };
  let constant = value => {
    open Ast_helper;
    let loc =
      Location.mknoloc(Longident.parse("Reconstruct.Route.Constant"));
    Exp.construct(
      loc,
      Some(Ast_helper.Exp.constant(Ast_helper.Const.string(value))),
    );
  };
  let wildcard = () => {
    open Ast_helper;
    let loc =
      Location.mknoloc(Longident.parse("Reconstruct.Route.Wildcard"));
    Exp.construct(loc, None);
  };
};

module ResultParts = {
  let toName: string => Ast_helper.str =
    name => {Asttypes.txt: name, loc: Ast_helper.default_loc^};
  let floatP = name => {
    open Ast_helper;
    let loc =
      Location.mknoloc(Longident.parse("Reconstruct.Route.FloatResult"));
    Pat.construct(loc, Some(Ast_helper.Pat.var(toName(name))));
  };
  let string = name => {
    open Ast_helper;
    let loc =
      Location.mknoloc(Longident.parse("Reconstruct.Route.StringResult"));
    Pat.construct(loc, Some(Ast_helper.Pat.var(toName(name))));
  };
  let int = name => {
    open Ast_helper;
    let loc =
      Location.mknoloc(Longident.parse("Reconstruct.Route.IntResult"));
    Pat.construct(loc, Some(Ast_helper.Pat.var(toName(name))));
  };
};

let createRouteAst = parsedRoute => {
  open Ast_helper;
  let listConstructorLocation = Location.mknoloc(Longident.parse("[]"));
  let consLocation = Location.mknoloc(Longident.parse("::"));
  let emptyListConstructor = Exp.construct(listConstructorLocation, None);
  let (<+>) = (newValue, prev) =>
    Exp.construct(consLocation, Some(Exp.tuple([newValue, prev])));
  open PathParts;
  let rec aux = route =>
    Reconstruct.Route.(
      switch (route) {
      | [] => emptyListConstructor
      | [Constant(value), ...tail] => constant(value) <+> aux(tail)
      | [String(name), ...tail] => string(name) <+> aux(tail)
      | [Int(name), ...tail] => int(name) <+> aux(tail)
      | [UInt(name), ...tail] => uint(name) <+> aux(tail)
      | [Float(name), ...tail] => floatP(name) <+> aux(tail)
      | [Wildcard, ...tail] => wildcard() <+> aux(tail)
      }
    );
  aux(parsedRoute);
};

let createRoutePattern = parsedRoute => {
  open Ast_helper;
  let listConstructorLocation = Location.mknoloc(Longident.parse("[]"));
  let consLocation = Location.mknoloc(Longident.parse("::"));
  let emptyListConstructor = Pat.construct(listConstructorLocation, None);
  let (<+>) = (newValue, prev) =>
    Pat.construct(consLocation, Some(Pat.tuple([newValue, prev])));
  open ResultParts;
  let name = count => "v" ++ string_of_int(count);
  let rec aux = (route, count) =>
    Reconstruct.Route.(
      switch (route) {
      | [] => emptyListConstructor
      | [String(_), ...tail] =>
        string(name(count)) <+> aux(tail, count + 1)
      | [Int(_), ...tail] => int(name(count)) <+> aux(tail, count + 1)
      | [UInt(_), ...tail] => int(name(count)) <+> aux(tail, count + 1)
      | [Float(_), ...tail] => floatP(name(count)) <+> aux(tail, count + 1)
      | [_, ...tail] => aux(tail, count)
      }
    );
  aux(parsedRoute, 0);
};

let createRouteApplication = parsedRoute => {
  open Ast_helper;
  let f = Exp.ident(Location.mknoloc(Longident.parse("f")));
  let ctx = (
    Asttypes.Nolabel,
    Exp.ident(Location.mknoloc(Longident.parse("ctx"))),
  );
  let identV_n = count =>
    Exp.ident(
      Location.mknoloc(Longident.parse("v" ++ string_of_int(count))),
    );
  let lbl =
    fun
    | "" => Asttypes.Nolabel
    | name => Asttypes.Labelled(name);
  let foldr = ((prev: list(_), i: int), item) =>
    switch (item) {
    | Reconstruct.Route.String(name) => (
        [(lbl(name), identV_n(i)), ...prev],
        i,
      )
    | Reconstruct.Route.Int(name) => (
        [(lbl(name), identV_n(i)), ...prev],
        i + 1,
      )
    | Reconstruct.Route.UInt(name) => (
        [(lbl(name), identV_n(i)), ...prev],
        i + 1,
      )
    | Reconstruct.Route.Float(name) => (
        [(lbl(name), identV_n(i)), ...prev],
        i + 1,
      )
    | _ => (prev, i)
    };
  let args =
    [ctx, ...List.fold_left(foldr, ([], 0), parsedRoute) |> fst] |> List.rev;
  Exp.apply(f, args);
};

let createRouteMachine = (~loc, parsedRoute) =>
  fun%expr (f, ctx) =>
    switch (
      Reconstruct.Route.evaluate(ctx, [%e createRouteAst(parsedRoute)])
    ) {
    | [%p createRoutePattern(parsedRoute)] =>
      %e
      createRouteApplication(parsedRoute)
    | a =>
      print_int(List.length(a));
      raise(
        Failure(
          "This expression should never execute. It means that there is a bug in the routing code",
        ),
      );
    | exception Reconstruct.Route.RouteDoesNotMatch =>
      Reconstruct.Machine.unhandled(ctx)
    };

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
      let parsedRoute = Reconstruct.Route.parse(str);
      createRouteMachine(~loc=e.pexp_loc, parsedRoute);
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