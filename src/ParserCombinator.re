/* The type of parsers */
type parser('a) = string => list(('a, string));

/* Primitive parsers */
let result : 'a => parser('a) =
    v => inp => [(v, inp)];

let zero : parser('a) =
    _inp => [];

let item : parser(char) =
    fun
    | "" => []
    | str => [(String.get(str, 0), String.sub(str, 1, String.length(str) - 1))];

/* Parser combinators */
let seq : parser('a) => parser('b) => parser(('a, 'b)) =
    (p, q) => inp =>
        p(inp)
        |> List.map(((x, inp')) =>
            q(inp')
            |> List.map(((y, inp'')) => ((x, y), inp'')))
        |> List.concat;

let bind : parser('a) => ('a => parser('b)) => parser('b) =
    (p, f) => inp =>
        p(inp)
        |> List.map(((v, inp')) => f(v, inp'))
        |> List.concat;


let seq' : parser('a) => parser('b) => parser(('a, 'b)) =
    (p, q) =>
        bind(p, x =>
        bind(q, y =>
        result((x, y))));

let sat : (char => bool) => parser(char) =
    p => bind(item, x => if (p(x)) { result(x) } else { zero });

let char : char => parser(char) =
    x => sat(y => y == x);

let digit : parser(char) =
    sat(x => '0' <= x && x <= '9');

let lower : parser(char) =
    sat(x => 'a' <= x && x <= 'z');

let upper : parser(char) =
    sat(x => 'A' <= x && x <= 'Z');

let plus : parser('a) => parser('a) => parser('a) =
    (p, q) => inp => (p(inp) @ q(inp));

let letter : parser(char) =
    plus(lower, upper);

let alphanum : parser(char) =
    plus(letter, digit);

let rec word : parser(string) = inp => {
    let neWord =
        bind(letter, x =>
        bind(word, xs =>
        result(String.concat("", [String.make(1, x), xs]))));
    plus(neWord, result(""), inp);
};
