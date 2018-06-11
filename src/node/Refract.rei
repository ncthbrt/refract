module Json: {
  exception JsonParseError(string);
  type t;
  type encoder('a) = 'a => t;
  type decoder('a) = t => 'a;
  module Decoder: {
    exception DecodeError(string, t, option(exn));
    let null: decoder(unit);
    let bool: decoder(bool);
    let string: decoder(string);
    let float: decoder(float);
    let assoc: decoder('a) => decoder(list((string, 'a)));
    let list: decoder('a) => decoder(list('a));
  };
};

module Method: {
  type t =
    | Get
    | Head
    | Post
    | Put
    | Delete
    | Connect
    | Options
    | Trace
    | Patch;
  let toString: t => string;
  let fromString: string => option(t);
};

module StatusCode: {
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
  let toInt: t => int;
  let fromInt: int => option(t);
};

module Protocol: {
  type t =
    | Http
    | Https;
  let toString: t => string;
  let fromString: string => option(t);
};

module HttpContext: {type t;};

module Prism: {
  type result;
  type t = HttpContext.t => Repromise.t(result);
  let handled: t;
  let unhandled: t;
  let unhandledWithError: exn => t;
};

module Request: {
  let post: Prism.t;
  let get: Prism.t;
  let put: Prism.t;
  let patch: Prism.t;
  let delete: Prism.t;
  let options: Prism.t;
  let method: (Method.t => Prism.t) => Prism.t;
  let isMethod: Method.t => Prism.t;
  let pathname: (list(string) => Prism.t) => Prism.t;
  let headers: (list((string, string)) => Prism.t) => Prism.t;
  let query: (list((string, option(string))) => Prism.t) => Prism.t;
  module Body: {
    let string: (string => Prism.t) => Prism.t;
    let json: (Json.decoder('a), 'a => Prism.t) => Prism.t;
  };
  module Pathname: {
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
    let matches: (t('a, Prism.t), 'a) => Prism.t;
  };
  module Query: {};
};

module Response: {
  let ok: Prism.t;
  let notFound: Prism.t;
  let status: StatusCode.t => Prism.t;
  module Body: {
    let string: string => Prism.t;
    let json: (Json.encoder('a), 'a) => Prism.t;
  };
};

let mapUnhandled:
  (
    Repromise.promise(Prism.result),
    option(exn) => Repromise.promise(Prism.result)
  ) =>
  Repromise.promise(Prism.result);

let map:
  (Repromise.t(Prism.result), Prism.t) => Repromise.promise(Prism.result);

let compose: (Prism.t, Prism.t) => Prism.t;

let composeMany: list(Prism.t) => Prism.t;

let switch_: list(Prism.t) => Prism.t;

let match_: list(Prism.t) => Prism.t;

let zip:
  (
    ('a => Prism.t) => Prism.t,
    ('b => Prism.t) => Prism.t,
    ('a, 'b) => Prism.t
  ) =>
  Prism.t;

module Server: {
  type t;
  let start: (~port: int=?, Prism.t) => t;
  let startSecure:
    (~privateKey: string, ~publicKey: string, ~port: int=?, Prism.t) => t;
};