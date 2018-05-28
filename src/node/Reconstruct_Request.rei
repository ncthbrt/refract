type t;

let method_: t => Reconstruct_Method.t;

let headers: t => list((string, string));

let url: t => string;