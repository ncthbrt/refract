module To_current =
  Migrate_parsetree.Convert(
    Migrate_parsetree.OCaml_402,
    Migrate_parsetree.OCaml_current,
  );

let migration =
  Migrate_parsetree.(
    Versions.migrate(Versions.ocaml_402, Versions.ocaml_current)
  );

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

module Pathname = {
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
      Location.mknoloc(Longident.parse("Refract.Request.Pathname.Float"));
    let string =
      Location.mknoloc(Longident.parse("Refract.Request.Pathname.String"));
    let int =
      Location.mknoloc(Longident.parse("Refract.Request.Pathname.Int"));
    let uint =
      Location.mknoloc(Longident.parse("Refract.Request.Pathname.UInt"));
    let constant =
      Location.mknoloc(Longident.parse("Refract.Request.Pathname.Constant"));
    let wildcard =
      Location.mknoloc(Longident.parse("Refract.Request.Pathname.Wildcard"));
    let custom =
      Location.mknoloc(Longident.parse("Refract.Request.Pathname.Custom"));
    let end_ =
      Location.mknoloc(Longident.parse("Refract.Request.Pathname.End"));
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
    | Post => fromName("Refract.Request.post")
    | Put => fromName("Refract.Request.put")
    | Delete => fromName("Refract.Request.delete")
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
      Pathname.(
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
      Ast_helper.Exp.apply(
        ~loc,
        makeIdent("pathHandlerFunc"),
        [("", makeIdentP(0))],
      )
    | args => Ast_helper.Exp.apply(~loc, makeIdent("pathHandlerFunc"), args)
    };
  };
  let createPathFunc = (~loc, arity, inner) => {
    open Ast_helper;
    let makePat = i =>
      Pat.var(~loc, Location.mknoloc("p" ++ string_of_int(i)));
    let makeUnitPat = () =>
      Pat.construct(~loc, Location.mknoloc(Longident.parse("()")), None);
    let rec aux = i =>
      Ast_helper.Exp.fun_(
        ~loc,
        "",
        None,
        i >= arity ? makeUnitPat() : makePat(i),
        i >= arity ? inner : aux(i + 1),
      );
    aux(0);
  };
  let create = (~loc: Ast_helper.loc, str) => {
    let path =
      try (Pathname.parse(str)) {
      | MalformedPathString(_) as e =>
        raise(MalformedPathStringWithLocation(e, loc))
      };
    let pathArity = Pathname.resultArity(path);
    let inner =
      Ast_helper.Exp.apply(
        ~loc,
        Ast_helper.Exp.ident(
          ~loc,
          Location.mknoloc(
            Longident.parse("Refract.Request.Pathname.matches"),
          ),
        ),
        [
          ("", Pathname.toExpr(path)),
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
      Ast_helper.Pat.var(~loc, Location.mknoloc("pathHandlerFunc")),
      inner,
    );
  };
  let createWithMethod = (~loc: Ast_helper.loc, method, str) =>
    Ast_helper.Exp.fun_(
      ~loc,
      "",
      None,
      Ast_helper.Pat.var(~loc, Location.mknoloc("pathHandlerFunc")),
      Ast_helper.Exp.apply(
        ~loc,
        Ast_helper.Exp.ident(
          ~loc,
          Location.mknoloc(Longident.parse("Refract.compose")),
        ),
        [
          ("", Method.toExpr(method)),
          (
            "",
            Ast_helper.Exp.apply(
              ~loc,
              create(~loc, str),
              [
                (
                  "",
                  Ast_helper.Exp.ident(
                    ~loc,
                    Location.mknoloc(Longident.parse("pathHandlerFunc")),
                  ),
                ),
              ],
            ),
          ),
        ],
      ),
    );
};

let createBoundPrism =
    (~loc, {expr, pat} as mapper: Ast_mapper.mapper, pattern, pvbExp, exp) => {
  open Asttypes;
  let pvbExp' = expr(mapper, pvbExp);
  let exp' = expr(mapper, exp);
  let pat' = pat(mapper, pattern);
  let fun_ = Ast_helper.Exp.fun_(~loc, "", None, pat', exp');
  Ast_helper.Exp.apply(~loc, pvbExp', [("", fun_)]);
};

let createComposedPrism =
    (~loc, {expr, pat} as mapper: Ast_mapper.mapper, e1, e2) => {
  let e1' = expr(mapper, e1);
  let e2' = expr(mapper, e2);
  Ast_helper.Exp.apply(
    ~loc,
    Ast_helper.Exp.ident(
      Location.mknoloc(Longident.parse("Refract.compose")),
    ),
    [("", e1'), ("", e2')],
  );
};

let mapper = {
  ...Ast_mapper.default_mapper,
  Ast_mapper.expr: (mapper, e) =>
    switch (e.pexp_desc) {
    | [@implicit_arity]
      Pexp_sequence(
        {
          pexp_desc:
            Pexp_extension((
              {Asttypes.txt: "refract.await", _},
              PStr([{pstr_desc: Pstr_eval(e1, _), _}]),
            )),
        },
        e2,
      ) =>
      createComposedPrism(~loc=e1.pexp_loc, mapper, e1, e2)
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
      Ast_mapper.default_mapper.expr(
        mapper,
        switch (extensionName) {
        | "refract" => Route.create(~loc=strLoc, str)
        | "refract.get" =>
          Route.createWithMethod(~loc=strLoc, Method.Get, str)
        | "refract.post" =>
          Route.createWithMethod(~loc=strLoc, Method.Post, str)
        | "refract.delete" =>
          Route.createWithMethod(~loc=strLoc, Method.Delete, str)
        | "refract.patch" =>
          Route.createWithMethod(~loc=strLoc, Method.Patch, str)
        | "refract.put" =>
          Route.createWithMethod(~loc=strLoc, Method.Put, str)
        | "refract.options" =>
          Route.createWithMethod(~loc=strLoc, Method.Options, str)
        | _ => Ast_mapper.default_mapper.expr(mapper, e)
        },
      )
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
      createBoundPrism(~loc, mapper, pat, pvb_expr, e')
    | _ => Ast_mapper.default_mapper.expr(mapper, e)
    },
};

let mapper = To_current.copy_mapper(mapper);

let () = {
  Location.register_error_of_exn(
    fun
    | MalformedPathStringWithLocation(MalformedPathString(reason), loc) =>
      Some(Location.error(~loc, reason))
    | _ => None,
    /* Migrate_parsetree.(
         Driver.register(
           ~name="ppx_refract", Versions.ocaml_402, (_config, _cookies) =>
           mapper
         )
       ); */
  );
  Migrate_parsetree.Compiler_libs.Ast_mapper.register("ppx_refract", arv =>
    mapper
  );
};