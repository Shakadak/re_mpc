type parser('a) = string => list(('a, string));

module Monad = {
    let pure : 'a => parser('a) =
        x => inp => [(x, inp)];

    let bind : parser('a) => ('a => parser('b)) => parser('b) =
                (p, f) => inp =>
                List.(p(inp)
                      |> map(((v, inp')) => f(v, inp'))
                      |> concat);
    let (>>=) = bind;

    let join : parser(parser('a)) => parser('a) =
        x => x >>= (y => y);

    let mzero : parser('a) =
        _inp => [];

    let mplus : parser('a) => parser('a) => parser('a) =
        (p, q) => inp => (p(inp) @ q(inp));

    let (+|+) = mplus;

    let mfilter : ('a => bool) => parser('a) => parser('a) =
        p => mx => mx >>= x => if (p(x)) { pure(x) } else { mzero };

    let (>?=) = mfilter;

    /*let liftM : ('a => 'b) => parser('a) => parser('b) =
        f => mx => mx >>= x => pure(f(x));*/
};

module Str = {
    let head : string => char =
        str => String.get(str, 0);
    let tail : string => string =
        str => String.sub(str, 1, String.length(str) - 1);
    let cons : char => string => string =
        c => str => String.concat("", [String.make(1, c), str]);
    let concat : list(string) => string =
        String.concat("");
    let rec explode : string => list(char) = fun
        | "" => []
        | str => [head(str), ...explode(tail(str))];
    let implode : list(char) => string =
        str => List.fold_right(cons, str, "");
};

/* Primitive parsers */
let item : parser(char) =
    fun
    | "" => []
    | str => [(Str.head(str), Str.tail(str))];

/* Parser combinators */
let seq : parser('a) => parser('b) => parser(('a, 'b)) =
    (p, q) => inp =>
    List.(
        p(inp)
        |> map(((x, inp')) =>
        q(inp') |> map(((y, inp'')) => ((x, y), inp'')))
        |> concat);

let seq' : parser('a) => parser('b) => parser(('a, 'b)) =
    (p, q) =>
        Monad.(
        p >>= x =>
        q >>= y =>
        pure((x, y)));

let sat : (char => bool) => parser(char) =
    p => Monad.(p >?= item);

let char : char => parser(char) =
    x => sat(y => y == x);

let digit : parser(char) =
    sat(x => '0' <= x && x <= '9');

let lower : parser(char) =
    sat(x => 'a' <= x && x <= 'z');

let upper : parser(char) =
    sat(x => 'A' <= x && x <= 'Z');

let letter : parser(char) =
    Monad.(lower +|+ upper);

let alphanum : parser(char) =
    Monad.(letter +|+ digit);

let rec word : parser(string) =
    inp =>
        Monad.(
            (letter >>= x =>
             word >>= xs =>
             pure(Str.cons(x, xs))) +|+ pure(""))(inp);

let rec string : string => parser(string) = fun
| "" => Monad.pure("")
| str => Monad.(
    char(Str.head(str)) >>= (_) =>
    string(Str.tail(str)) >>= (_) =>
    pure(str));

let rec many : parser('a) => parser(list('a)) =
    p =>
    Monad.(
        (p >>= x =>
        many(p) >>= xs =>
        pure([x, ...xs])) +|+ pure([]));

let word' = Monad.(many(letter) >>= x => pure(Str.implode));

let ident : parser(string) =
    Monad.(
        lower >>= x =>
        many(alphanum) >>= xs =>
        pure(Str.implode([x, ...xs])));

let many1 : parser('a) => parser(list('a)) =
    p =>
    Monad.(
        p >>= x =>
        many(p) >>= xs =>
        pure([x, ...xs]));


let nat : parser(int) =
    Monad.(many1(digit) >>= xs => pure(int_of_string(Str.implode(xs))));

let int : parser(int) =
    Monad.(
        (char('-') >>= (_) =>
         nat >>= x =>
         pure(-x)) +|+ nat);

let int' = {
    let op = Monad.((char('-') >>= (_) => pure(x => -x)) +|+ pure(x => x));
    Monad.(op >>= f => nat >>= n => pure(f(n)));
};

let ints : parser(list(int)) =
    Monad.(
        char('[') >>= (_) =>
        int >>= x =>
        many(char(',') >>= (_) => int >>= x => pure(x)) >>= xs =>
        char(']') >>= (_) =>
        pure([x, ...xs]));
