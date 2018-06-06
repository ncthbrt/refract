module type RefractString = {
  let uppercaseAscii: string => string;
  let splitOnChar: (char, string) => list(string);
};

module Method = {
  type t =
    /* Has no request body */
    | Get
    /* Has no request body and no response body */
    | Head
    | Post
    /* Has no response body and is idempotent */
    | Put
    /* May have both req/res body and is idempotent */
    | Delete
    /* No req body and has res body */
    | Connect
    /* No req body and has res body, safe, idempotent */
    | Options
    /* No req body and no res body */
    | Trace
    /* Has req body and no res body */
    | Patch;
  let fromString = str =>
    switch (str) {
    | "GET" => Some(Get)
    | "HEAD" => Some(Head)
    | "POST" => Some(Post)
    | "PUT" => Some(Put)
    | "DELETE" => Some(Delete)
    | "CONNECT" => Some(Connect)
    | "OPTIONS" => Some(Options)
    | "TRACE" => Some(Trace)
    | "PATCH" => Some(Patch)
    | _ => None
    };
  let toString =
    fun
    | Get => "GET"
    | Head => "HEAD"
    | Post => "POST"
    | Put => "PUT"
    | Delete => "DELETE"
    | Connect => "CONNECT"
    | Options => "OPTIONS"
    | Trace => "TRACE"
    | Patch => "PATCH";
};

module StatusCode = {
  type t =
    | Ok
    | Created
    | Accepted
    | NonAuthoritativeInformation
    | NoContent
    | ResetContent
    | PartialContent
    | MultiStatus
    | AleadyReported
    | IMUsed
    | MultipleChoices
    | MovedPermanently
    | Found
    | SeeOther
    | NotModified
    | UseProxy
    | SwitchProxy
    | TemporaryRedirect
    | PermanentRedirect
    | BadRequest
    | Unauthorized
    | PaymentRequired
    | Forbidden
    | NotFound
    | MethodNotAllowed
    | NotAcceptable
    | ProxyAuthenticationRequired
    | RequestTimeout
    | Conflict
    | Gone
    | LengthRequired
    | PreconditionFailed
    | PayloadTooLarge
    | UriTooLong
    | UnsupportedMediaType
    | RangeNotSatisfiable
    | ExpectationFailed
    | ImATeapot
    | MisdirectedRequest
    | UnprocessableEntity
    | Locked
    | FailedDependency
    | UpgradeRequired
    | PreconditionRequired
    | TooManyRequests
    | RequestHeaderFieldsTooLarge
    | UnavailableForLegalReasons
    | InternalServerError
    | NotImplemented
    | BadGateway
    | ServiceUnavailable
    | GatewayTimeout
    | HttpVersionNotSupported
    | VariantAlsoNegotiates
    | InsufficientStorage
    | LoopDetected
    | NotExtended
    | NetworkAuthenticationRequired;
  let fromInt =
    fun
    | 200 => Some(Ok)
    | 201 => Some(Created)
    | 202 => Some(Accepted)
    | 203 => Some(NonAuthoritativeInformation)
    | 204 => Some(NoContent)
    | 205 => Some(ResetContent)
    | 206 => Some(PartialContent)
    | 207 => Some(MultiStatus)
    | 208 => Some(AleadyReported)
    | 226 => Some(IMUsed)
    | 300 => Some(MultipleChoices)
    | 301 => Some(MovedPermanently)
    | 302 => Some(Found)
    | 303 => Some(SeeOther)
    | 304 => Some(NotModified)
    | 305 => Some(UseProxy)
    | 306 => Some(SwitchProxy)
    | 307 => Some(TemporaryRedirect)
    | 308 => Some(PermanentRedirect)
    | 400 => Some(BadRequest)
    | 401 => Some(Unauthorized)
    | 402 => Some(PaymentRequired)
    | 403 => Some(Forbidden)
    | 404 => Some(NotFound)
    | 405 => Some(MethodNotAllowed)
    | 406 => Some(NotAcceptable)
    | 407 => Some(ProxyAuthenticationRequired)
    | 408 => Some(RequestTimeout)
    | 409 => Some(Conflict)
    | 410 => Some(Gone)
    | 411 => Some(LengthRequired)
    | 412 => Some(PreconditionFailed)
    | 413 => Some(PayloadTooLarge)
    | 414 => Some(UriTooLong)
    | 415 => Some(UnsupportedMediaType)
    | 416 => Some(RangeNotSatisfiable)
    | 417 => Some(ExpectationFailed)
    | 418 => Some(ImATeapot)
    | 421 => Some(MisdirectedRequest)
    | 422 => Some(UnprocessableEntity)
    | 423 => Some(Locked)
    | 424 => Some(FailedDependency)
    | 426 => Some(UpgradeRequired)
    | 428 => Some(PreconditionRequired)
    | 429 => Some(TooManyRequests)
    | 431 => Some(RequestHeaderFieldsTooLarge)
    | 451 => Some(UnavailableForLegalReasons)
    | 500 => Some(InternalServerError)
    | 501 => Some(NotImplemented)
    | 502 => Some(BadGateway)
    | 503 => Some(ServiceUnavailable)
    | 504 => Some(GatewayTimeout)
    | 505 => Some(HttpVersionNotSupported)
    | 506 => Some(VariantAlsoNegotiates)
    | 507 => Some(InsufficientStorage)
    | 508 => Some(LoopDetected)
    | 510 => Some(NotExtended)
    | 511 => Some(NetworkAuthenticationRequired)
    | _ => None;
  let toInt =
    fun
    | Ok => 200
    | Created => 201
    | Accepted => 202
    | NonAuthoritativeInformation => 203
    | NoContent => 204
    | ResetContent => 205
    | PartialContent => 206
    | MultiStatus => 207
    | AleadyReported => 208
    | IMUsed => 226
    | MultipleChoices => 300
    | MovedPermanently => 301
    | Found => 302
    | SeeOther => 303
    | NotModified => 304
    | UseProxy => 305
    | SwitchProxy => 306
    | TemporaryRedirect => 307
    | PermanentRedirect => 308
    | BadRequest => 400
    | Unauthorized => 401
    | PaymentRequired => 402
    | Forbidden => 403
    | NotFound => 404
    | MethodNotAllowed => 405
    | NotAcceptable => 406
    | ProxyAuthenticationRequired => 407
    | RequestTimeout => 408
    | Conflict => 409
    | Gone => 410
    | LengthRequired => 411
    | PreconditionFailed => 412
    | PayloadTooLarge => 413
    | UriTooLong => 414
    | UnsupportedMediaType => 415
    | RangeNotSatisfiable => 416
    | ExpectationFailed => 417
    | ImATeapot => 418
    | MisdirectedRequest => 421
    | UnprocessableEntity => 422
    | Locked => 423
    | FailedDependency => 424
    | UpgradeRequired => 426
    | PreconditionRequired => 428
    | TooManyRequests => 429
    | RequestHeaderFieldsTooLarge => 431
    | UnavailableForLegalReasons => 451
    | InternalServerError => 500
    | NotImplemented => 501
    | BadGateway => 502
    | ServiceUnavailable => 503
    | GatewayTimeout => 504
    | HttpVersionNotSupported => 505
    | VariantAlsoNegotiates => 506
    | InsufficientStorage => 507
    | LoopDetected => 508
    | NotExtended => 510
    | NetworkAuthenticationRequired => 511;
};

module RefractResponse = {
  type body = [
    | `Empty
    | `String(string)
    | `Stream((string => Repromise.t(unit)) => Repromise.t(unit))
  ];
  type t = {
    headers: list((string, string)),
    status: StatusCode.t,
    body,
  };
  let empty = {headers: [], status: StatusCode.NotFound, body: `Empty};
  let status = (res, statusCode: StatusCode.t) => {
    ...res,
    status: statusCode,
  };
  module Body = {
    let string: (t, string) => t =
      (res, str) => {...res, body: `String(str)};
  };
};

module type RefractRequest = {
  type t;
  module Body: {let string: t => Repromise.t(string);};
  let method_: t => Method.t;
  let url: t => string;
  let headers: t => list((string, string));
};

module type RefractJson = {type t; let tryParse: string => t;};

module Make = (RefractRequest: RefractRequest, RefractString: RefractString) => {
  module Method = Method;
  module StatusCode = StatusCode;
  module HttpContext = {
    type t = {
      request: RefractRequest.t,
      response: RefractResponse.t,
    };
    let make = request : t => {request, response: RefractResponse.empty};
  };
  module Prism = {
    type result =
      | Unhandled(option(exn))
      | Handled(HttpContext.t);
    type t = HttpContext.t => Repromise.t(result);
    let handled: t = ctx => Repromise.resolve(Handled(ctx));
    let unhandled: t =
      (_: HttpContext.t) => Repromise.resolve(Unhandled(None));
    let unhandledWithError: exn => t =
      (err, _) => Repromise.resolve(Unhandled(Some(err)));
  };
  module Request = {
    module Query = {};
    module Path = {
      exception RouteDoesNotMatch;
      exception MalformedQueryParameter(string, string, exn);
      type t('func, 'result) =
        | End: t(unit => 'result, 'result)
        | Constant(string, t('func, 'result)): t('func, 'result)
        | String(t('func, 'result)): t(string => 'func, 'result)
        | UInt(t('func, 'result)): t(int => 'func, 'result)
        | Int(t('func, 'result)): t(int => 'func, 'result)
        | Float(t('func, 'result)): t(float => 'func, 'result)
        | Wildcard(t('func, 'result)): t('func, 'result)
        | Custom(string => 'a, t('func, 'result)): t('a => 'func, 'result);
      let split: HttpContext.t => list(string) =
        ctx =>
          List.filter(
            fun
            | "" => false
            | _ => true,
            RefractString.splitOnChar(
              '/',
              List.hd(
                RefractString.splitOnChar(
                  '?',
                  RefractRequest.url(ctx.request),
                ),
              ),
            ),
          );
      let swizzle = (f, a, b) => f(b, a);
      let rec evalPath:
        type func result. (func, t(func, result), list(string)) => result =
        (f, route, parts) =>
          switch (route, parts) {
          | (End, []) => f()
          | (_, []) => raise(RouteDoesNotMatch)
          | (End, _) => raise(RouteDoesNotMatch)
          | (Constant(value, tl), [str, ...next]) when value == str =>
            evalPath(f, tl, next)
          | (Constant(_), _) => raise(RouteDoesNotMatch)
          | (String(tl), [str, ...next]) => evalPath(f(str), tl, next)
          | (Int(tl), [str, ...next]) =>
            let value =
              try (int_of_string(str)) {
              | Failure(_) => raise(RouteDoesNotMatch)
              };
            evalPath(f(value), tl, next);
          | (UInt(tl), [str, ...next]) =>
            let value =
              try (int_of_string(str)) {
              | Failure(_) => raise(RouteDoesNotMatch)
              };
            if (value < 0) {
              raise(RouteDoesNotMatch);
            } else {
              evalPath(f(value), tl, next);
            };
          | (Float(tl), [str, ...next]) =>
            let value =
              try (float_of_string(str)) {
              | Failure(_) => raise(RouteDoesNotMatch)
              };
            evalPath(f(value), tl, next);
          | (Wildcard(tl), [_, ...next]) =>
            try (evalPath(f, tl, next)) {
            | RouteDoesNotMatch => evalPath(f, Wildcard(tl), next)
            }
          | (Custom(parser, tl), [str, ...next]) =>
            let value =
              try (parser(str)) {
              | _ => raise(RouteDoesNotMatch)
              };
            evalPath(f(value), tl, next);
          };
      let matches: type func. (t(func, Prism.t), func) => Prism.t =
        (path, f, ctx) => {
          let pathParts = split(ctx);
          (
            try (evalPath(f, path, pathParts)) {
            | RouteDoesNotMatch => Prism.unhandled
            }
          )(
            ctx,
          );
        };
    };
    module Body = {
      let string = (f: string => Prism.t, ctx: HttpContext.t) => {
        let str = RefractRequest.Body.string(ctx.request);
        Repromise.then_(str => f(str, ctx), str);
      };
    };
    let method: (Method.t => Prism.t) => Prism.t =
      (f, ctx) => f(RefractRequest.method_(ctx.request), ctx);
    let isMethod: Method.t => Prism.t =
      (method, ctx) =>
        RefractRequest.method_(ctx.request) == method ?
          Prism.handled(ctx) : Prism.unhandled(ctx);
    let get = isMethod(Method.Get);
    let post = isMethod(Method.Post);
    let put = isMethod(Method.Put);
    let patch = isMethod(Method.Patch);
    let delete = isMethod(Method.Delete);
    let options = isMethod(Method.Options);
    let url: (string => Prism.t) => Prism.t =
      (f, ctx) => f(RefractRequest.url(ctx.request), ctx);
    let headers = (f, ctx: HttpContext.t) =>
      f(RefractRequest.headers(ctx.request), ctx);
  };
  module Response = {
    let ok: Prism.t =
      ctx =>
        Prism.handled({
          ...ctx,
          response: RefractResponse.status(ctx.response, StatusCode.Ok),
        });
    let notFound: Prism.t =
      ctx =>
        Prism.handled({
          ...ctx,
          response: RefractResponse.status(ctx.response, StatusCode.NotFound),
        });
    let status: StatusCode.t => Prism.t =
      (status, ctx) =>
        Prism.handled({
          ...ctx,
          response: RefractResponse.status(ctx.response, status),
        });
    module Body = {
      let string: string => Prism.t =
        (str, ctx) =>
          Prism.handled({
            ...ctx,
            response: RefractResponse.Body.string(ctx.response, str),
          });
    };
  };
  let mapUnhandled = (res, f) =>
    Repromise.then_(
      fun
      | Prism.Unhandled(e) => f(e)
      | Handled(ctx) => Repromise.resolve(Prism.Handled(ctx)),
      res,
    );
  let map = (res: Repromise.t(Prism.result), f: Prism.t) =>
    Repromise.then_(
      fun
      | Prism.Handled(ctx) => f(ctx)
      | Unhandled(e) => Repromise.resolve(Prism.Unhandled(e)),
      res,
    );
  let compose: (Prism.t, Prism.t) => Prism.t =
    (a, b, ctx) => map(a(ctx), b);
  let rec composeMany: list(Prism.t) => Prism.t =
    (prisms, ctx) =>
      switch (prisms) {
      | [] => Prism.unhandled(ctx)
      | [prism] => prism(ctx)
      | [prism, ...prisms] => compose(prism, composeMany(prisms), ctx)
      };
  let switch_: list(Prism.t) => Prism.t =
    (lst, ctx: HttpContext.t) => {
      let rec aux =
        fun
        | [] => Prism.unhandled(ctx)
        | [hd, ...tail] =>
          mapUnhandled(hd(ctx), x =>
            switch (x) {
            | None => aux(tail)
            | Some(e) => Repromise.resolve(Prism.Unhandled(Some(e)))
            }
          );
      aux(lst);
    };
  let match_ = switch_;
  let zip:
    (
      ('a => Prism.t) => Prism.t,
      ('b => Prism.t) => Prism.t,
      ('a, 'b) => Prism.t
    ) =>
    Prism.t =
    (ma, mb, f, ctx) => ma(a => mb(f(a)), ctx);
};