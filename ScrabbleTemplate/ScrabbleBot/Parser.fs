// ScrabbleUtil contains the types coord, boardProg, and SquareProg. Remove these from your file before proceeding.
// Also note that the modulse Ass7 and ImpParser have been merged to one module called Parser.

// Insert your Parser.fs file here from Assignment 7. All modules must be internal.

module internal Parser

    open StateMonad
    open ScrabbleUtil // NEW. KEEP THIS LINE.
    open Eval
    open FParsecLight.TextParser     // Industrial parser-combinator library. Use for Scrabble Project.
    
    let pIntToChar = pstring "intToChar"

    let pPointValue = pstring "pointValue"

    let pCharToInt  = pstring "charToInt"
    let pToUpper    = pstring "toUpper"
    let pToLower    = pstring "toLower"
    let pCharValue  = pstring "charValue"

    let pTrue       = pstring "true"
    let pFalse      = pstring "false"
    let pIsDigit    = pstring "isDigit"
    let pIsLetter   = pstring "isLetter"
    
    let pif       = pstring "if"
    let pthen     = pstring "then"
    let pelse     = pstring "else"
    let pwhile    = pstring "while"
    let pdo       = pstring "do"
    
    let pIsVowel   = pstring  "isVowel"
    let pdeclare = pstring "declare"
    let whitespaceChar : Parser<char> = satisfy (System.Char.IsWhiteSpace) <?> "whitespace"
    let pletter: Parser<char> = satisfy (System.Char.IsLetter) <?> "letter"
    let palphanumeric: Parser<char> = satisfy (System.Char.IsLetterOrDigit) <?> "alphanumeric"

    let spaces = many whitespaceChar <?> "spaces"
    let spaces1 = many1 whitespaceChar <?> "space1"

    (* Methods to skip spaces *)
    let (.>*>.) (p1 : Parser<'a>) (p2 : Parser<'b>) : Parser<'a * 'b> = 
        (p1 .>> spaces .>>. p2)
    
    let (.>*>) (p1 : Parser<'a>) (p2 : Parser<'b>) : Parser<'a> =
        (p1 .>> spaces .>> p2)

    let (>*>.) (p1 : Parser<'a>) (p2 : Parser<'b>) : Parser<'b> = 
        p1 >>. spaces >>. p2
    
    (* Skip parenthesise *)
    let parenthesise (p: Parser<'a>): Parser<'a> = 
        pchar '(' >*>. p .>*> pchar ')'

    (* Skip curly brackets *)
    let parenthesise1 (p: Parser<'a>): Parser<'a> = 
        pchar '{' >*>. p .>*> pchar '}'
        
    (*
    //auxillary function takes a char list and concats these to a string
    //parse a letter or a underscore first
    //then parse a number, a letter or a digit and "sends" it to the auxillary function
    *)
    let pid : Parser<string> =
        let aux (charlst: char list) = charlst |> System.String.Concat
        pletter <|> pchar '_' .>>. many (palphanumeric <|> pchar '_') |>> fun (x,y) -> x :: y |> aux 
 
    let unop (op: Parser<'a>) (a: Parser<'b>) : Parser<'b> =
        op >*>. a
    let binop (op: Parser<'a>) (a: Parser<'b>) (b: Parser<'c>) : Parser<'b*'c> =
        (a .>*> op) .>*>. b

    //Arithmic expressions
    let TermParse, tref = createParserForwardedToRef<aExp>()
    let ProdParse, pref = createParserForwardedToRef<aExp>()
    let AtomParse, aref = createParserForwardedToRef<aExp>()
    let CTomParse, cref = createParserForwardedToRef<cExp>()
    
    //Term parse
    let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"
    let SubParse = binop (pchar '-') ProdParse TermParse |>> Sub <?> "Sub"
    do tref := choice [AddParse; SubParse; ProdParse]

    //Product parse
    let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"
    let DivParse = binop (pchar '/') AtomParse ProdParse |>> Div <?> "Div"
    let ModParse = binop (pchar '%') AtomParse ProdParse |>> Mod <?> "Mod"
    do pref := choice [MulParse; DivParse; ModParse; AtomParse]

    //Atom parse
    let NParse   = pint32 |>> N <?> "Int"
    let ParParse = parenthesise TermParse <?> "parParse"
    let NegParse = unop (pchar '-') AtomParse |>> (fun x -> (N -1, x)) |>> Mul <?> "Neg"
    let VarParse = pid |>> V <?> "Variable"
    let PointParse = unop pPointValue AtomParse |>> PV <?> "PointValue"
    let charToInt = unop pCharToInt CTomParse |>> CharToInt <?> "charToInt"

    do aref := choice [NegParse; PointParse; charToInt; ParParse; VarParse; NParse;]
    let AexpParse = TermParse

    //charExpressions
    let CParse = (pchar ''') >>. anyChar .>> (pchar ''') |>> C <?> "C"
    let charValParse = unop pCharValue AtomParse |>> CV <?> "charValue"
    let intToCharParse = unop pIntToChar AtomParse |>> IntToChar <?> "intToChar"
    let toUpperParse = unop pToUpper CTomParse |>> ToUpper <?> "toUpper"
    let toLowerParse = unop pToLower CTomParse |>> ToLower <?> "toLower"
    let ParCParse = parenthesise CTomParse <?> "parParse"

    do cref := choice [charValParse; intToCharParse; toUpperParse; toLowerParse; ParCParse; CParse]
    let CexpParse = CTomParse

    //Boolean expressions
    let BTermParse, btref = createParserForwardedToRef<bExp>()
    let BProdParse, bpref = createParserForwardedToRef<bExp>()
    let BtomParse,  bref = createParserForwardedToRef<bExp>()
   
    //BTermParse
    let ConjParse = binop (pstring "/\\") BProdParse BTermParse |>> Conj <?> "conjunction"
    let DisjParse = binop (pstring "\\/") BProdParse BTermParse |>> (fun x -> (Not (fst x), Not (snd x))) |>> Conj |>> Not <?> "disjunction"
    btref:= choice [ConjParse; DisjParse; BProdParse]
    
    //BProdParse
    let AEqualParse = binop (pchar '=') AtomParse ProdParse |>> (fun (x,y)  -> x .=. y)  <?> "equality"
    let ANEqualParse = binop (pstring "<>") AtomParse ProdParse |>> (fun (x,y)  -> x .<>. y) <?> "inequality"
    let ALessThanParse = binop (pchar '<') AtomParse ProdParse |>> ALt <?> "lessThan"
    let AMoreOrEqualParse = binop (pstring ">=") AtomParse ProdParse |>> (fun (x,y)  -> x .>=. y) <?> "moreOrEquealTo"
    let AMoreThanParse = binop (pchar '>') AtomParse ProdParse |>> (fun (x,y)  -> x .>. y) <?> "moreThan"                                                                                   
    let ALessOrEqualParse = binop (pstring "<=") AtomParse ProdParse |>> (fun (x,y)  -> x .<=. y) <?> "lessOrEquealTo"
    bpref := choice [AEqualParse; ANEqualParse; ALessThanParse; AMoreOrEqualParse; AMoreThanParse; ALessOrEqualParse; BtomParse]
    
    //BTomparse
    let trueParse = pTrue |>> (fun _ -> TT)  <?> "true"
    let falseParse = pFalse |>> (fun _ -> FF) <?> "false"
    let NotParser = unop (pchar '~') BtomParse |>> Not <?> "not"
    let isVowelParse = unop pIsVowel CTomParse |>> IsVowel <?> "isDigit"
    let parBParse = parenthesise BTermParse <?> "parParse"
    bref := choice [NotParser; trueParse; falseParse; isVowelParse; parBParse]
    
    let BexpParse = BTermParse

    //Statement parse
    let SParse, sref = createParserForwardedToRef<stm>()
    let SeqParse, sqref = createParserForwardedToRef<stm>()
    
    //Sequence parse
    let sSParse = binop (pchar ';') SParse SeqParse |>> Seq <?> "S"
    sqref := choice [sSParse; SParse]
    
    //SParse
    let dontKnowParse = binop (pstring(":=")) pid AexpParse |>> Ass <?> "dontknow"
    let declareParse = pdeclare >>. spaces1 >>. pid |>> Declare <?> "declare"
    let ifThenElse = pif >*>. parenthesise BexpParse .>*> pthen .>*>. parenthesise1 SeqParse .>*> pelse .>*>.  parenthesise1 SeqParse |>> (fun x -> (fst (fst x), snd (fst x), snd x)) |>> ITE <?> "ifThenElse"
    let ifThen = pif >*>. parenthesise BexpParse .>*> pthen .>*>. parenthesise1 SeqParse |>> (fun x -> (fst x, snd x, Skip)) |>> ITE <?> "ifThen"
    let whileParse = pwhile >*>. parenthesise BexpParse .>*> pdo .>*>. parenthesise1 SeqParse |>> While <?> "While"
    
    sref := choice [dontKnowParse; declareParse; ifThenElse; ifThen; whileParse]
    
    let stmParse = SeqParse

    (* The rest of your parser goes here *)
    type word   = (char * int) list
    type squareFun = word -> int -> int -> Result<int, Error>
    type square = Map<int, squareFun>
    
    type boardFun2 = coord -> Result<square option, Error>
        
    type board = {
        center        : coord
        defaultSquare : square
        squares       : boardFun2
    }
    
    // Default (unusable) board in case you are not implementing a parser for the DSL.
    // let mkBoard : boardProg -> board = fun _ -> {center = (0,0); defaultSquare = Map.empty; squares = fun _ -> Success (Some Map.empty)}
    
    let parseSquareProg (sqp: squareProg) =
        sqp |> Map.map(fun _ str -> run stmParse str |> getSuccess |> stmntToSquareFun)

    // a board function takes:  coord -> Result<square option, Error>
    // let stmntToBoardFun (stmnt: stmnt) (t: Map<int, 'a>) ((x,y):coord): Result<'a option, Error> =
    // Therefore this with partial application should return a function that takes a coord
    // using ||> to pass two arguments to the function stmntToBoardFun
    let parseBoardProg (s: string) (sqs: Map<int,square>): boardFun2 =
        let stm = run stmParse s |> getSuccess
        (stm, sqs) ||> stmntToBoardFun 

    let mkBoard (bp : boardProg) : board =
        let m = bp.squares |> Map.map(fun x y -> parseSquareProg y) 
        {
            center = bp.center;
            defaultSquare = m.[bp.usedSquare]
            squares = parseBoardProg bp.prog m
        }

