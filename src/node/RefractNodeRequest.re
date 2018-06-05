type t = RefractNode.Request.t;

let method_ = req =>
  RefractCommon.Method.fromString(
    RefractNode.Request.methodAsStr(req) |. Js.String.toUpperCase,
  )
  |. Belt.Option.getExn;

let headers = req => {
  let rec aux = (prev, headers) =>
    switch (headers) {
    | [k, v, ...tail] => aux([(k, v), ...prev], tail)
    | [] => prev
    | [k] => [(k, ""), ...prev]
    };
  aux([], RefractNode.Request.rawHeaders(req) |. Belt.List.fromArray);
};

let httpVersion = req => RefractNode.Request.httpVersion(req);

let url = req => RefractNode.Request.url(req);

module Body = {
  let string = req => {
    let (promise: Repromise.t(_), resolve) = Repromise.new_();
    let body = [||];
    RefractNode.Request.on(
      req,
      `data(buffer => ignore(Js.Array.push(buffer, body))),
    );
    RefractNode.Request.on(req, `error((_) => ()));
    RefractNode.Request.on(
      req,
      `end_(
        (_) =>
          resolve(RefractNode.Buffer.concat(body) |. Node.Buffer.toString),
      ),
    );
    promise;
  };
};