module Ast = Ast_406;

module Ast_helper = Ast.Ast_helper;

module Ast_mapper = Ast.Ast_mapper;

module Asttypes = Ast.Asttypes;

module Location = Ast.Location;

module Longident = Ast.Longident;

module Parsetree = Ast.Parsetree;

open Ast_mapper;

open Parsetree;

exception MalformedRouteStringWithLocation(exn, Location.t);

let validateName =
  fun
  | "" => true
  | name =>
    Refract_CrossplatString.matches(~regex="^[a-z_][0-9a-zA-Z_']*$", name);

module Path = {
  let cons = (loc, nameOrValue) =>
    Ast_helper.Exp.construct(
      loc,
      Some(Ast_helper.Exp.constant(Ast_helper.Const.string(nameOrValue))),
    );
  let floatP = name => {
    let loc = Location.mknoloc(Longident.parse("Refract.Route.Float"));
    cons(loc, name);
  };
  let string = name => {
    let loc = Location.mknoloc(Longident.parse("Refract.Route.String"));
    cons(loc, name);
  };
  let int = name => {
    let loc = Location.mknoloc(Longident.parse("Refract.Route.Int"));
    cons(loc, name);
  };
  let uint = name => {
    let loc = Location.mknoloc(Longident.parse("Refract.Route.UInt"));
    cons(loc, name);
  };
  let constant = value => {
    let loc = Location.mknoloc(Longident.parse("Refract.Route.Constant"));
    cons(loc, value);
  };
  let wildcard = () => {
    open Ast_helper;
    let loc = Location.mknoloc(Longident.parse("Refract.Route.Wildcard"));
    Exp.construct(loc, None);
  };
};

module Query = {
  let trueLoc = Location.mknoloc(Longident.parse("true"));
  let falseLoc = Location.mknoloc(Longident.parse("false"));
  let boolExp =
    fun
    | true => Ast_helper.Exp.construct(trueLoc, None)
    | false => Ast_helper.Exp.construct(falseLoc, None);
  let cons = (loc, name, isOptional) =>
    Ast_helper.(
      Exp.construct(
        loc,
        Some(
          Exp.tuple([
            Exp.constant(Ast_helper.Const.string(name)),
            boolExp(isOptional),
          ]),
        ),
      )
    );
  let floatP = (name, isOptional: bool) => {
    let loc = Location.mknoloc(Longident.parse("Refract.Route.FloatQuery"));
    cons(loc, name, isOptional);
  };
  let string = (name, isOptional) => {
    let loc = Location.mknoloc(Longident.parse("Refract.Route.StringQuery"));
    cons(loc, name, isOptional);
  };
  let int = (name, isOptional) => {
    let loc = Location.mknoloc(Longident.parse("Refract.Route.IntQuery"));
    cons(loc, name, isOptional);
  };
  let uint = (name, isOptional) => {
    let loc = Location.mknoloc(Longident.parse("Refract.Route.UIntQuery"));
    cons(loc, name, isOptional);
  };
  let bool = (name, isOptional) => {
    let loc = Location.mknoloc(Longident.parse("Refract.Route.BoolQuery"));
    cons(loc, name, isOptional);
  };
  let flag = name => {
    open Ast_helper;
    let loc = Location.mknoloc(Longident.parse("Refract.Route.FlagQuery"));
    Exp.construct(loc, Some(Exp.constant(Ast_helper.Const.string(name))));
  };
  let toExp =
    Refract.Route.(
      fun
      | FlagQuery(name) => flag(name)
      | BoolQuery(name, isOptional) => bool(name, isOptional)
      | StringQuery(name, isOptional) => string(name, isOptional)
      | IntQuery(name, isOptional) => int(name, isOptional)
      | UIntQuery(name, isOptional) => uint(name, isOptional)
      | FloatQuery(name, isOptional) => floatP(name, isOptional)
    );
};

module PathResult = {
  let toName: string => Ast_helper.str =
    name => {Asttypes.txt: name, loc: Ast_helper.default_loc^};
  let floatP = name => {
    open Ast_helper;
    let loc = Location.mknoloc(Longident.parse("Refract.Route.FloatResult"));
    Pat.construct(loc, Some(Ast_helper.Pat.var(toName(name))));
  };
  let string = name => {
    open Ast_helper;
    let loc =
      Location.mknoloc(Longident.parse("Refract.Route.StringResult"));
    Pat.construct(loc, Some(Ast_helper.Pat.var(toName(name))));
  };
  let int = name => {
    open Ast_helper;
    let loc = Location.mknoloc(Longident.parse("Refract.Route.IntResult"));
    Pat.construct(loc, Some(Ast_helper.Pat.var(toName(name))));
  };
};

module QueryResult = {
  let someLoc = Location.mknoloc(Longident.parse("Some"));
  let toName: string => Ast_helper.str =
    name => {Asttypes.txt: name, loc: Ast_helper.default_loc^};
  let cons = (loc, name, isOptional) =>
    Ast_helper.(
      if (isOptional) {
        Pat.construct(loc, Some(Pat.var(toName(name))));
      } else {
        Pat.construct(
          loc,
          Some(Pat.construct(someLoc, Some(Pat.var(toName(name))))),
        );
      }
    );
  let floatP = (name, isOptional) => {
    let loc =
      Location.mknoloc(Longident.parse("Refract.Route.FloatQueryResult"));
    cons(loc, name, isOptional);
  };
  let string = (name, isOptional) => {
    let loc =
      Location.mknoloc(Longident.parse("Refract.Route.StringQueryResult"));
    cons(loc, name, isOptional);
  };
  let int = (name, isOptional) => {
    let loc =
      Location.mknoloc(Longident.parse("Refract.Route.IntQueryResult"));
    cons(loc, name, isOptional);
  };
  let bool = (name, isOptional) => {
    let loc =
      Location.mknoloc(Longident.parse("Refract.Route.BoolQueryResult"));
    cons(loc, name, isOptional);
  };
  let flag = bool(_, false);
};

let createRouteAst = (parsedRoute: Refract.Route.t) => {
  open Ast_helper;
  let listConstructorLocation = Location.mknoloc(Longident.parse("[]"));
  let consLocation = Location.mknoloc(Longident.parse("::"));
  let emptyListConstructor = Exp.construct(listConstructorLocation, None);
  let (<+>) = (newValue, prev) =>
    Exp.construct(consLocation, Some(Exp.tuple([newValue, prev])));
  let rec path = (parts: list(Refract.Route.path)) =>
    Path.(
      Refract.Route.(
        switch (parts) {
        | [] => emptyListConstructor
        | [Constant(value), ...tail] => constant(value) <+> path(tail)
        | [String(name), ...tail] => string(name) <+> path(tail)
        | [Int(name), ...tail] => int(name) <+> path(tail)
        | [UInt(name), ...tail] => uint(name) <+> path(tail)
        | [Float(name), ...tail] => floatP(name) <+> path(tail)
        | [Wildcard, ...tail] => wildcard() <+> path(tail)
        }
      )
    );
  let rec query = (parts: list(Refract.Route.query)) =>
    Query.(
      switch (parts) {
      | [] => emptyListConstructor
      | [v, ...tail] => toExp(v) <+> query(tail)
      }
    );
  Ast_helper.Exp.tuple([path(fst(parsedRoute)), query(snd(parsedRoute))]);
};

let createRoutePattern = (parsedRoute: Refract.Route.t) => {
  open Ast_helper;
  let listConstructorLocation = Location.mknoloc(Longident.parse("[]"));
  let consLocation = Location.mknoloc(Longident.parse("::"));
  let emptyListConstructor = Pat.construct(listConstructorLocation, None);
  let (<+>) = (newValue, prev) =>
    Pat.construct(consLocation, Some(Pat.tuple([newValue, prev])));
  let pName = count => "p" ++ string_of_int(count);
  let qName = count => "q" ++ string_of_int(count);
  let rec path = (parts: list(Refract.Route.path), count) =>
    PathResult.(
      Refract.Route.(
        switch (parts) {
        | [] => emptyListConstructor
        | [String(_), ...tail] =>
          string(pName(count)) <+> path(tail, count + 1)
        | [Int(_), ...tail] => int(pName(count)) <+> path(tail, count + 1)
        | [UInt(_), ...tail] => int(pName(count)) <+> path(tail, count + 1)
        | [Float(_), ...tail] =>
          floatP(pName(count)) <+> path(tail, count + 1)
        | [_, ...tail] => path(tail, count)
        }
      )
    );
  let rec query = (parts: list(Refract.Route.query), count) =>
    QueryResult.(
      Refract.Route.(
        switch (parts) {
        | [FlagQuery(_), ...tail] =>
          flag(qName(count)) <+> query(tail, count + 1)
        | [BoolQuery(_, isOptional), ...tail] =>
          bool(qName(count), isOptional) <+> query(tail, count + 1)
        | [StringQuery(_, isOptional), ...tail] =>
          string(qName(count), isOptional) <+> query(tail, count + 1)
        | [IntQuery(_, isOptional), ...tail] =>
          int(qName(count), isOptional) <+> query(tail, count + 1)
        | [UIntQuery(_, isOptional), ...tail] =>
          int(qName(count), isOptional) <+> query(tail, count + 1)
        | [FloatQuery(_, isOptional), ...tail] =>
          floatP(qName(count), isOptional) <+> query(tail, count + 1)
        | [] => emptyListConstructor
        }
      )
    );
  Pat.tuple([path(fst(parsedRoute), 0), query(snd(parsedRoute), 0)]);
};

let createRouteApplication = (parsedRoute: Refract.Route.t) => {
  open Ast_helper;
  let f = Exp.ident(Location.mknoloc(Longident.parse("f")));
  let pName = count =>
    Exp.ident(
      Location.mknoloc(Longident.parse("p" ++ string_of_int(count))),
    );
  let qName = count =>
    Exp.ident(
      Location.mknoloc(Longident.parse("q" ++ string_of_int(count))),
    );
  let lbl =
    fun
    | "" => Asttypes.Nolabel
    | name => Asttypes.Labelled(name);
  let lblOpt = isOptional =>
    fun
    | "" => Asttypes.Nolabel
    | name => isOptional ? Asttypes.Optional(name) : Asttypes.Labelled(name);
  let foldPath = ((prev: list(_), i: int), item: Refract.Route.path) =>
    switch (item) {
    | Refract.Route.String(name) => ([(lbl(name), pName(i)), ...prev], i)
    | Refract.Route.Int(name) => (
        [(lbl(name), pName(i)), ...prev],
        i + 1,
      )
    | Refract.Route.UInt(name) => (
        [(lbl(name), pName(i)), ...prev],
        i + 1,
      )
    | Refract.Route.Float(name) => (
        [(lbl(name), pName(i)), ...prev],
        i + 1,
      )
    | _ => (prev, i)
    };
  let foldQuery = ((prev: list(_), i: int), item: Refract.Route.query) =>
    switch (item) {
    | Refract.Route.StringQuery(name, isOptional) => (
        [(lblOpt(isOptional, name), qName(i)), ...prev],
        i,
      )
    | Refract.Route.IntQuery(name, isOptional) => (
        [(lblOpt(isOptional, name), qName(i)), ...prev],
        i + 1,
      )
    | Refract.Route.UIntQuery(name, isOptional) => (
        [(lblOpt(isOptional, name), qName(i)), ...prev],
        i + 1,
      )
    | Refract.Route.FloatQuery(name, isOptional) => (
        [(lblOpt(isOptional, name), qName(i)), ...prev],
        i + 1,
      )
    | Refract.Route.BoolQuery(name, isOptional) => (
        [(lblOpt(isOptional, name), qName(i)), ...prev],
        i + 1,
      )
    | Refract.Route.FlagQuery(name) => (
        [(lbl(name), qName(i)), ...prev],
        i + 1,
      )
    };
  let args = List.fold_left(foldPath, ([], 0), fst(parsedRoute)) |> fst;
  let args' = List.fold_left(foldQuery, (args, 0), snd(parsedRoute)) |> fst;
  Exp.apply(f, args' |> List.rev);
};

let createRouteMachine = (~loc: Ast_helper.loc, parsedRoute) =>
  fun%expr (f, ctx: Refract.HttpContext.t) =>
    switch (Refract.Route.evaluate(ctx, [%e createRouteAst(parsedRoute)])) {
    | [%p createRoutePattern(parsedRoute)] =>
      [%e createRouteApplication(parsedRoute)](ctx)
    | _ =>
      raise(
        Failure(
          "This expression should never execute. It means that there is a bug in the routing code",
        ),
      )
    | exception Refract.Route.RouteDoesNotMatch =>
      Refract.Machine.unhandled(ctx)
    };

let createRouteMachineWithMethod = (~loc: Ast_helper.loc, method, parsedRoute) =>
  fun%expr (f, ctx: Refract.HttpContext.t) =>
    switch (Refract.Route.evaluate(ctx, [%e createRouteAst(parsedRoute)])) {
    | [%p createRoutePattern(parsedRoute)] =>
      Refract.compose(
        [%e method],
        [%e createRouteApplication(parsedRoute)],
        ctx,
      )
    | _ =>
      raise(
        Failure(
          "This expression should never execute. It means that there is a bug in the routing code",
        ),
      )
    | exception Refract.Route.RouteDoesNotMatch =>
      Refract.Machine.unhandled(ctx)
    };

let createBoundMachine = (mapper: Ast_mapper.mapper, pat, pvbExp, exp, loc) => {
  open Asttypes;
  let pvbExp' = mapper.expr(mapper, pvbExp);
  let exp' = mapper.expr(mapper, exp);
  let pat' = mapper.pat(mapper, pat);
  let fun_ = fun%expr ([%p pat']) => [%e exp'];
  %expr
  [%e pvbExp']([%e fun_]);
};

let mapper = {
  ...default_mapper,
  expr: (mapper, e) =>
    switch (e.pexp_desc) {
    | Pexp_extension((
        {Asttypes.txt: extensionName, _},
        PStr([
          {
            pstr_desc:
              Pstr_eval(
                {
                  pexp_desc: Pexp_constant(Pconst_string(str, _)),
                  pexp_loc: strLoc,
                  _,
                },
                _,
              ),
            _,
          },
        ]),
      )) =>
      switch (extensionName) {
      | "route" =>
        try (createRouteMachine(~loc=e.pexp_loc, Refract.Route.parse(str))) {
        | Refract.Route.MalformedRouteString(_) as e =>
          raise(MalformedRouteStringWithLocation(e, strLoc))
        }
      | "route.get" =>
        try (
          createRouteMachineWithMethod(
            ~loc=e.pexp_loc,
            Ast_helper.Exp.ident(
              Location.mknoloc(Longident.parse("Refract.get")),
            ),
            Refract.Route.parse(str),
          )
        ) {
        | Refract.Route.MalformedRouteString(_) as e =>
          raise(MalformedRouteStringWithLocation(e, strLoc))
        }
      | "route.post" =>
        try (
          createRouteMachineWithMethod(
            ~loc=e.pexp_loc,
            Ast_helper.Exp.ident(
              Location.mknoloc(Longident.parse("Refract.post")),
            ),
            Refract.Route.parse(str),
          )
        ) {
        | Refract.Route.MalformedRouteString(_) as e =>
          raise(MalformedRouteStringWithLocation(e, strLoc))
        }
      | "route.delete" =>
        try (
          createRouteMachineWithMethod(
            ~loc=e.pexp_loc,
            Ast_helper.Exp.ident(
              Location.mknoloc(Longident.parse("Refract.delete")),
            ),
            Refract.Route.parse(str),
          )
        ) {
        | Refract.Route.MalformedRouteString(_) as e =>
          raise(MalformedRouteStringWithLocation(e, strLoc))
        }
      | "route.patch" =>
        try (
          createRouteMachineWithMethod(
            ~loc=e.pexp_loc,
            Ast_helper.Exp.ident(
              Location.mknoloc(Longident.parse("Refract.patch")),
            ),
            Refract.Route.parse(str),
          )
        ) {
        | Refract.Route.MalformedRouteString(_) as e =>
          raise(MalformedRouteStringWithLocation(e, strLoc))
        }
      | "route.put" =>
        try (
          createRouteMachineWithMethod(
            ~loc=e.pexp_loc,
            Ast_helper.Exp.ident(
              Location.mknoloc(Longident.parse("Refract")),
            ),
            Refractte.parse(str),
          )
        ) {
        | Refract.Route.MalformedRouteString(_) as e =>
          raise(MalformedRouteStringWithLocation(e, strLoc))
        }
      | _ => default_mapper.expr(mapper, e)
      }
    | [@implicit_arity]
      Pexp_extension(
        {Asttypes.txt: "mesh", _},
        PStr([
          {
            pstr_desc:
              Pstr_eval(
                {
                  pexp_desc:
                    Pexp_let(
                      Asttypes.Nonrecursive,
                      [{pvb_pat: pat, pvb_loc: loc, pvb_expr, _}],
                      e',
                    ),
                  _,
                },
                _,
              ),
            _,
          },
        ]),
      ) =>
      createBoundMachine(mapper, pat, pvb_expr, e', loc)
    | _ => default_mapper.expr(mapper, e)
    },
};

let () = {
  Location.register_error_of_exn(
    fun
    | MalformedRouteStringWithLocation(
        Refract.Route.MalformedRouteString(reason),
        loc,
      ) =>
      Some(Location.error(~loc, reason))
    | _ => None,
  );
  Migrate_parsetree.(
    Driver.register(
      ~name="ppx_Refract", Versions.ocaml_406, (_config, _cookies) =>
      mapper
    )
  );
};