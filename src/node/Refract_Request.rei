type t;

let method_: t => Refract_Method.t;

let headers: t => list((string, string));

let url: t => string;

module Body: {let string: t => Repromise.t(string);};