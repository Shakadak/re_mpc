// Generated by BUCKLESCRIPT VERSION 2.2.3, PLEASE EDIT WITH CARE
'use strict';

var List = require("bs-platform/lib/js/list.js");
var Curry = require("bs-platform/lib/js/curry.js");
var $$String = require("bs-platform/lib/js/string.js");
var Pervasives = require("bs-platform/lib/js/pervasives.js");
var Caml_string = require("bs-platform/lib/js/caml_string.js");

function result(v, inp) {
  return /* :: */[
          /* tuple */[
            v,
            inp
          ],
          /* [] */0
        ];
}

function zero() {
  return /* [] */0;
}

function item(str) {
  if (str === "") {
    return /* [] */0;
  } else {
    return /* :: */[
            /* tuple */[
              Caml_string.get(str, 0),
              $$String.sub(str, 1, str.length - 1 | 0)
            ],
            /* [] */0
          ];
  }
}

function seq(p, q, inp) {
  return List.concat(List.map((function (param) {
                    var x = param[0];
                    return List.map((function (param) {
                                  return /* tuple */[
                                          /* tuple */[
                                            x,
                                            param[0]
                                          ],
                                          param[1]
                                        ];
                                }), Curry._1(q, param[1]));
                  }), Curry._1(p, inp)));
}

function bind(p, f, inp) {
  return List.concat(List.map((function (param) {
                    return Curry._2(f, param[0], param[1]);
                  }), Curry._1(p, inp)));
}

function seq$prime(p, q) {
  return (function (param) {
      return bind(p, (function (x) {
                    return (function (param) {
                        return bind(q, (function (y) {
                                      var partial_arg = /* tuple */[
                                        x,
                                        y
                                      ];
                                      return (function (param) {
                                          return result(partial_arg, param);
                                        });
                                    }), param);
                      });
                  }), param);
    });
}

function sat(p) {
  return (function (param) {
      return bind(item, (function (x) {
                    if (Curry._1(p, x)) {
                      return (function (param) {
                          return result(x, param);
                        });
                    } else {
                      return zero;
                    }
                  }), param);
    });
}

function $$char(x) {
  return sat((function (y) {
                return +(y === x);
              }));
}

var digit = sat((function (x) {
        if (/* "0" */48 <= x) {
          return +(x <= /* "9" */57);
        } else {
          return /* false */0;
        }
      }));

var lower = sat((function (x) {
        if (/* "a" */97 <= x) {
          return +(x <= /* "z" */122);
        } else {
          return /* false */0;
        }
      }));

var upper = sat((function (x) {
        if (/* "A" */65 <= x) {
          return +(x <= /* "Z" */90);
        } else {
          return /* false */0;
        }
      }));

function plus(p, q, inp) {
  return Pervasives.$at(Curry._1(p, inp), Curry._1(q, inp));
}

function letter(param) {
  return plus(lower, upper, param);
}

function alphanum(param) {
  return plus(letter, digit, param);
}

exports.result = result;
exports.zero = zero;
exports.item = item;
exports.seq = seq;
exports.bind = bind;
exports.seq$prime = seq$prime;
exports.sat = sat;
exports.$$char = $$char;
exports.digit = digit;
exports.lower = lower;
exports.upper = upper;
exports.plus = plus;
exports.letter = letter;
exports.alphanum = alphanum;
/* digit Not a pure module */