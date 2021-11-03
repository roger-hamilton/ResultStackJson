open FSharpPlus

type JsonValue =
    | JsonNull
    | JsonBool of bool
    | JsonNumber of float
    | JsonString of string
    | JsonArray of JsonValue list
    | JsonObject of Map<string, JsonValue>


type Parser<'a> =
    | Parser of (string -> ('a * string) option)
    static member Run (Parser p: Parser<'a>) inp = p inp

    // Functor
    static member Map(Parser p: Parser<'a>, f) =
        Parser (fun inp ->
            monad {
                let! x, inp' = p inp
                f x, inp'
            })

    // Applicative
    static member Return x = Parser(fun inp -> Some(x, inp))

    static member (<*>)(Parser pf, Parser px) =
        let inner inp =
            monad {
                let! f, inp' = pf inp
                let! x, inp'' = px inp'
                f x, inp''
            }

        Parser inner

    // Alternative
    static member get_Empty() = Parser(konst None)

    static member (<|>)(Parser p1, Parser p2) = Parser(fun inp -> p1 inp <|> p2 inp)


let uncons (str: string) =
    if length str = 0 then
        None
    else
        Some(str.[0], str.[1..])


let parseIf pred =
    Parser (fun inp ->
        match uncons inp with
        | Some (c', inp') when pred c' -> Some(c', inp')
        | _ -> None)

let charP c = parseIf ((=) c)

// we can also use a special operator in FSharpPlus (<!>)
// to make calls to map easier (this is the same as <$> in Haskell)
let strP (str: char seq) = // seq<char> -> Parser<string>
    String.ofSeq <!> sequence (map charP str)

// Like map, but ignores the value in the functor and just injects value
// (Equivalent to <$ in Haskell)
let inline (<!) value functor = konst value <!> functor

let jsonNull = JsonNull <! strP "null" // Parser<JsonValue>

let jsonTrue = JsonBool true <! strP "true" // Parser<JsonValue>
let jsonFalse = JsonBool false <! strP "false" // Parser<JsonValue>

let jsonBool = jsonTrue <|> jsonFalse // Parser<JsonValue>


// matches zero or more instances of the Parser
let many (Parser p) =
    let rec inner inp =
        match p inp with
        | Some (x, inp') ->
            let xs, inp'' = inner inp'
            x :: xs, inp''
        | None -> [], inp

    Parser(inner >> Some)

// matches one or more instances of the Parser
let some (Parser p) =
    let (Parser manyP) = many (Parser p)

    let inner inp =
        monad {
            let! x, inp' = p inp
            let! xs, inp'' = manyP inp'
            return x :: xs, inp''
        }

    Parser inner

let digits =
    String.ofSeq
    <!> some (parseIf System.Char.IsDigit)

let doubleLit =
    let es = result ""
    let minus = strP "-"
    let fract = ((+) <!> strP "." <*> digits)

    let exp =
        (fun e s d -> e + s + d)
        <!> (strP "e" <|> strP "E")
        <*> (strP "+" <|> strP "-" <|> es)
        <*> digits

    (fun s w f e -> s + w + f + e) <!> (minus <|> es)
    <*> digits
    <*> (fract <|> es)
    <*> (exp <|> es)


let jsonNumber = float >> JsonNumber <!> doubleLit

let stringLit =
    String.ofSeq
    <!> charP '"' *> many (parseIf ((<>) '"'))
    <* charP '"'

let jsonString = JsonString <!> stringLit

let forwardParser<'a> () = // unit -> ref<Parser<'a>> * Parser<'a>
    let parserRef: Parser<'a> ref = ref (empty)

    let parser: Parser<'a> =
        Parser(
            (fun inp ->
                let (Parser p') = parserRef.Value
                p' inp)
        )

    parserRef, parser

let jsonValueRef, jsonValue = forwardParser<JsonValue> ()

// One or more matces of el seperated by sep
let sepBy (sep: Parser<'a>) (el: Parser<'b>) =
    (fun x xs -> x :: xs) <!> el <*> many (sep *> el)

// consumes any whitespace
let ws = many (parseIf System.Char.IsWhiteSpace)

// sepBy - specialized for comma seperation
let commaSep p =
    sepBy (ws *> charP ',' <* ws) p <|> result []

let jsonArray =
    JsonArray
    <!> charP '[' *> ws *> commaSep jsonValue
    <* ws
    <* charP ']'

let jsonObject =
    let pair =
        tuple2 <!> stringLit <* ws <* charP ':' <* ws
        <*> jsonValue

    JsonObject
    <!> Map.ofList
    <!> charP '{' *> ws *> commaSep pair
    <* ws
    <* charP '}'

jsonValueRef.Value <-
    jsonNull
    <|> jsonBool
    <|> jsonNumber
    <|> jsonString
    <|> jsonArray
    <|> jsonObject

printfn "%A" (Parser.Run jsonValue "null")
printfn "%A" (Parser.Run jsonValue "true")
printfn "%A" (Parser.Run jsonValue "false")
printfn "%A" (Parser.Run jsonValue "1234")
printfn "%A" (Parser.Run jsonValue "-1234")
printfn "%A" (Parser.Run jsonValue "-1234.05")
printfn "%A" (Parser.Run jsonValue "-1234.05e-2")
printfn "%A" (Parser.Run jsonValue "\"hello world\"")

printfn
    "%A"
    (Parser.Run jsonValue "[null, true,\t\tfalse,\n1234,\"hello world\",[ ], { }, {\"foo\": true,\n\"bar\": -3}]")
