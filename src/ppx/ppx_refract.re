module Ast = Ast_402;

module Ast_helper = Ast.Ast_helper;

module Ast_mapper = Ast.Ast_mapper;

module Asttypes = Ast.Asttypes;

module Location = Ast.Location;

module Longident = Ast.Longident;

module Parsetree = Ast.Parsetree;

open Parsetree;

exception MalformedPathString(string);

exception MalformedQueryString(string);

exception MalformedPathStringWithLocation(exn, Location.t);

module Path = {
  let identifierRe = Re.Posix.re("([a-z_][0-9a-zA-Z_']*)?");
  let typeNameOrConversionFunctionRe =
    Re.Posix.re("([A-Za-z][0-9a-zA-Z_']*\\.)*([a-z_][0-9a-zA-Z_']*)");
  let pathPartRe =
    Tyre.(
      compile(
        str("{")
        *> (
          regex(identifierRe)
          <&> str(":")
          *> regex(typeNameOrConversionFunctionRe)
        )
        <* str("}"),
      )
    );
  type token =
    | Constant(string)
    | String(string)
    | Int(string)
    | UInt(string)
    | Float(string)
    | Wildcard
    | Custom(string, string);
  let parse: string => list(token) =
    str => {
      let parts =
        List.filter(x => x != "", Str.split(Str.regexp("/+"), str));
      let rec aux =
        fun
        | [] => []
        | [hd, ...tail] =>
          if (hd == "*") {
            [Wildcard, ...aux(tail)];
          } else {
            switch (Tyre.exec(pathPartRe, hd)) {
            | Result.Ok((name, "string")) => [String(name), ...aux(tail)]
            | Result.Ok((name, "int")) => [Int(name), ...aux(tail)]
            | Result.Ok((name, "uint")) => [UInt(name), ...aux(tail)]
            | Result.Ok((name, "float")) => [Float(name), ...aux(tail)]
            | Result.Ok((name, func)) => [Custom(name, func), ...aux(tail)]
            | Result.Error(_) => [Constant(hd), ...aux(tail)]
            };
          };
      aux(parts);
    };
  module Locs = {
    let float =
      Location.mknoloc(Longident.parse("Refract.Request.Path.Float"));
    let string =
      Location.mknoloc(Longident.parse("Refract.Request.Path.String"));
    let int = Location.mknoloc(Longident.parse("Refract.Request.Path.Int"));
    let uint =
      Location.mknoloc(Longident.parse("Refract.Request.Path.UInt"));
    let constant =
      Location.mknoloc(Longident.parse("Refract.Request.Path.Constant"));
    let wildcard =
      Location.mknoloc(Longident.parse("Refract.Request.Path.Wildcard"));
    let custom =
      Location.mknoloc(Longident.parse("Refract.Request.Path.Custom"));
    let end_ = Location.mknoloc(Longident.parse("Refract.Request.Path.End"));
    let fromName = name => Location.mknoloc(Longident.parse(name));
    let getLoc =
      fun
      | Constant(_) => constant
      | String(_) => string
      | Int(_) => int
      | UInt(_) => uint
      | Float(_) => float
      | Wildcard => wildcard
      | Custom(_, _) => custom;
  };
  let rec resultArity: list(token) => int =
    fun
    | [] => 0
    | [Constant(_), ...tail]
    | [Wildcard, ...tail] => resultArity(tail)
    | [_, ...tail] => 1 + resultArity(tail);
  let rec toExpr =
    fun
    | [] => Ast_helper.Exp.construct(Locs.end_, None)
    | [hd, ...tl] => {
        let loc = Locs.getLoc(hd);
        let constr = Ast_helper.Exp.construct(loc);
        switch (hd) {
        | Constant(value) =>
          constr(
            Some(
              Ast_helper.Exp.tuple([
                Ast_helper.Exp.constant(Asttypes.Const_string(value, None)),
                toExpr(tl),
              ]),
            ),
          )
        | Custom(_, f) =>
          constr(
            Some(
              Ast_helper.Exp.tuple([
                Ast_helper.Exp.ident(Locs.fromName(f)),
                toExpr(tl),
              ]),
            ),
          )
        | _ => constr(Some(toExpr(tl)))
        };
      };
};

module Method = {
  type t =
    | Get
    | Post
    | Put
    | Delete
    | Options
    | Patch;
  let fromName = name =>
    Ast_helper.(Exp.ident(Location.mknoloc(Longident.parse(name))));
  let rec toExpr =
    fun
    | Get => fromName("Refract.Request.get")
    | Post => fromName("Refract.Method.post")
    | Put => fromName("Refract.Method.put")
    | Delete => fromName("Refract.Method.delete")
    | Patch => fromName("Refract.Request.patch")
    | Options => fromName("Refract.Request.options");
};

module Route = {
  let createApplication = (~loc, path) => {
    open Ast_helper;
    let makeIdent = name =>
      Exp.ident(Location.mknoloc(Longident.parse(name)));
    let makeIdentP = i => makeIdent("p" ++ string_of_int(i));
    let rec aux =
      Path.(
        i =>
          fun
          | [] => []
          | [Constant(_), ...tl] => aux(i, tl)
          | [Wildcard, ...tl] => aux(i, tl)
          | [String(""), ...tl] => [
              ("", makeIdentP(i)),
              ...aux(i + 1, tl),
            ]
          | [Int(""), ...tl] => [("", makeIdentP(i)), ...aux(i + 1, tl)]
          | [UInt(""), ...tl] => [("", makeIdentP(i)), ...aux(i + 1, tl)]
          | [Float(""), ...tl] => [("", makeIdentP(i)), ...aux(i + 1, tl)]
          | [Custom("", _), ...tl] => [
              ("", makeIdentP(i)),
              ...aux(i + 1, tl),
            ]
          | [String(name), ...tl] => [
              (name, makeIdentP(i)),
              ...aux(i + 1, tl),
            ]
          | [Int(name), ...tl] => [
              (name, makeIdentP(i)),
              ...aux(i + 1, tl),
            ]
          | [UInt(name), ...tl] => [
              (name, makeIdentP(i)),
              ...aux(i + 1, tl),
            ]
          | [Float(name), ...tl] => [
              (name, makeIdentP(i)),
              ...aux(i + 1, tl),
            ]
          | [Custom(name, _), ...tl] => [
              (name, makeIdentP(i)),
              ...aux(i + 1, tl),
            ]
      );
    let args = aux(0, path);
    switch (args) {
    | [] =>
      Ast_helper.Exp.apply(~loc, makeIdent("f"), [("", makeIdentP(0))])
    | args => Ast_helper.Exp.apply(~loc, makeIdent("f"), args)
    };
  };
  let createPathFunc = (~loc, arity, inner) => {
    open Ast_helper;
    let makePat = i =>
      Pat.var(~loc, Location.mknoloc("p" ++ string_of_int(i)));
    let rec aux = i =>
      Ast_helper.Exp.fun_(
        ~loc,
        "",
        None,
        makePat(i),
        i >= arity ? inner : aux(i + 1),
      );
    aux(0);
  };
  let create = (~loc: Ast_helper.loc, str) => {
    let path =
      try (Path.parse(str)) {
      | MalformedPathString(_) as e =>
        raise(MalformedPathStringWithLocation(e, loc))
      };
    let pathArity = Path.resultArity(path);
    let inner =
      Ast_helper.Exp.apply(
        ~loc,
        Ast_helper.Exp.ident(
          ~loc,
          Location.mknoloc(Longident.parse("Refract.Request.Path.matches")),
        ),
        [
          ("", Path.toExpr(path)),
          (
            "",
            createPathFunc(~loc, pathArity, createApplication(~loc, path)),
          ),
        ],
      );
    Ast_helper.Exp.fun_(
      ~loc,
      "",
      None,
      Ast_helper.Pat.var(~loc, Location.mknoloc("f")),
      inner,
    );
  };
  let createWithMethod = (~loc: Ast_helper.loc, method, str) =>
    Ast_helper.Exp.fun_(
      ~loc,
      "",
      None,
      Ast_helper.Pat.var(~loc, Location.mknoloc("f")),
      Ast_helper.Exp.apply(
        ~loc,
        Ast_helper.Exp.ident(
          ~loc,
          Location.mknoloc(Longident.parse("Refract.compose")),
        ),
        [("", Method.toExpr(method)), ("", create(~loc, str))],
      ),
    );
};

let createBoundPrism = (mapper: Ast_mapper.mapper, pat, pvbExp, exp, loc) => {
  open Asttypes;
  let pvbExp' = mapper.expr(mapper, pvbExp);
  let exp' = mapper.expr(mapper, exp);
  let pat' = mapper.pat(mapper, pat);
  let fun_ = Ast_helper.Exp.fun_(~loc, "", None, pat', exp');
  Ast_helper.Exp.apply(~loc, pvbExp, [("", fun_)]);
};

let mapper = {
  ...Ast_mapper.default_mapper,
  expr: (mapper, e) =>
    switch (e.pexp_desc) {
    | Pexp_extension((
        {Asttypes.txt: extensionName, _},
        PStr([
          {
            pstr_desc:
              Pstr_eval(
                {
                  pexp_desc: Pexp_constant(Asttypes.Const_string(str, _)),
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
      | "refract" => Route.create(~loc=strLoc, str)
      | "refract.get" => Route.createWithMethod(~loc=strLoc, Method.Get, str)
      | "refract.post" =>
        Route.createWithMethod(~loc=strLoc, Method.Post, str)
      | "refract.delete" =>
        Route.createWithMethod(~loc=strLoc, Method.Delete, str)
      | "refract.patch" =>
        Route.createWithMethod(~loc=strLoc, Method.Patch, str)
      | "refract.put" => Route.createWithMethod(~loc=strLoc, Method.Put, str)
      | "refract.options" =>
        Route.createWithMethod(~loc=strLoc, Method.Options, str)
      | _ => Ast_mapper.default_mapper.expr(mapper, e)
      }
    | [@implicit_arity]
      Pexp_extension(
        {Asttypes.txt: "refract", _},
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
      createBoundPrism(mapper, pat, pvb_expr, e', loc)
    | _ => Ast_mapper.default_mapper.expr(mapper, e)
    },
};

let () = {
  Location.register_error_of_exn(
    fun
    | MalformedPathStringWithLocation(MalformedPathString(reason), loc) =>
      Some(Location.error(~loc, reason))
    | _ => None,
  );
  Migrate_parsetree.(
    Driver.register(
      ~name="ppx_Refract", Versions.ocaml_402, (_config, _cookies) =>
      mapper
    )
  );
};