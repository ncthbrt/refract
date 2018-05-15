type t;

type req;

type res;

[@bs.module "http2"] external createServer : ((req, res) => unit) => t = "";

[@bs.module "http2"]
external createSecureServer :
  {
    .
    "key": string,
    "cert": string,
  } =>
  t =
  "";

[@bs.send] external listen : (t, int) => unit = "";

let start = (~port=3000, machine) => {
  let server =
    createServer((req, res) => {
      let context: HttpContext.t = {request: req, response: res};
      ();
    });
  listen(server, port);
  server;
};

let startSecure = (~privateKey, ~publicKey, ~port=3000, machine) => {
  let server = createSecureServer({"key": privateKey, "cert": publicKey});
  listen(server, port);
  server;
};