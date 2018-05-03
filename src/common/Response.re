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

module Body = {
  type t =
    | String(string)
    | Bytes(bytes);
};

type t = {
  status: StatusCode.t,
  headers: list((string, string)),
};