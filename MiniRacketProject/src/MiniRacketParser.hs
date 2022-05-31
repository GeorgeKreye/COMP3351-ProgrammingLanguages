module MiniRacketParser where
    import Parser
    import Expr
    import Control.Applicative
    import Error (ErrorT)

    parseBool :: Parser Bool
    parseBool =
        do parseKeyword "true" >>return True
        <|> do parseKeyword "false" >> return False

    -- parsing bool operations, these are 'and' and 'or'
    parseBoolOp :: Parser BoolOp
    parseBoolOp =
        do parseKeyword "and" >> return And
        <|> do parseKeyword "or" >> return Or

    -- parse math operations and return the MathOp
    parseMathOp :: Parser MathOp
    parseMathOp =
        do symbol "+" >> return Add
        <|> do symbol "-" >> return Sub
        <|> do symbol "*" >> return Mul
        <|> do symbol "div" >> return Div
        <|> do symbol "mod" >> return Mod

    -- parse the comp operations and return the CompOp
    parseCompOp :: Parser CompOp
    parseCompOp =
        do symbol "<=" >> return Leq
        <|> do symbol ">=" >> return Geq
        <|> do symbol "<" >> return Lt
        <|> do symbol ">" >> return Gt
        <|> do symbol "equal?" >> return Eq

    -- a literal in MiniRacket is true, false, or a number
    literal :: Parser Value
    literal =
        do BoolVal <$> parseBool
        <|> do IntVal <$> natural

    -- parse a literal expression, which at this point, is just a literal
    literalExpr :: Parser Expr
    literalExpr =
        do LiteralExpr <$> literal

    -- keywords for keyword parsing
    keywordList :: [String]
    keywordList = ["false", "true", "not", "and", "or"]

    -- try to parse a keyword, otherwise it's a variable, this can be
    -- used to check if the identifier we see (i.e., variable name) is
    -- actually a keyword, which isn't legal
    parseKeyword :: String -> Parser String
    parseKeyword keyword = do
        -- all keywords follow the identifier rules, so we'll use that
        name <- identifier
        if name `elem` keywordList && keyword == name
            then return name
            else failParse $ "saw " ++ name ++ ", expected " ++ keyword

    -- parses not expressions
    notExpr :: Parser Expr
    notExpr = do parseKeyword "not" >> NotExpr <$> parseExpr

    -- parse a var expression, here we need to make sure that
    -- the identifier is *not* a keyword before accepting it
    -- i.e., we fail the parse if it is     
    varExpr :: Parser Expr
    varExpr = do
        name <- identifier
        if name `elem` keywordList
            then failParse (name ++ " is a keyword name, so it cannot be a variable name")
            else return $ VarExpr name

    -- a bool expression is the operator followed by one or more expressions that we have to parse
    boolExpr :: Parser Expr
    boolExpr =  BoolExpr <$> parseBoolOp <*> kstar parseExpr

    -- a math expression is the operator followed by one or more expressions that we have to parse
    mathExpr :: Parser Expr
    mathExpr = MathExpr <$> parseMathOp <*> kstar parseExpr

    -- a comp expression is the comp operator and the parsing of two expressions
    compExpr :: Parser Expr
    compExpr = CompExpr <$> parseCompOp <*> parseExpr <*> parseExpr

    -- parse an if-expression, which begins with the keyword if,
    -- and is followed by three expressions
    ifExpr :: Parser Expr
    ifExpr = do
        parseKeyword "if"
        IfExpr <$> parseExpr <*> parseExpr <*> parseExpr

    --TODO: Implement applyExpr
    -- what we do know is that the left argument will result in a function,
    -- otherwise we'll have an error, but nesting them like this allows us
    -- to further build up functions
    applyExpr :: Parser Expr
    applyExpr = do
        failParse "Not implemented"

    -- a let expression begins with the keyword let, followed by
    -- parenthesis which contains an identifier for the name 
    -- to be bound, an expression to bind to that name, a close
    -- parenthesis, and a body  
    letExpr :: Parser Expr
    letExpr = do
        symbol "let"
        symbol "("
        v <- varExpr
        a <- parseExpr
        symbol ")"
        b <- parseExpr
        case v of
            VarExpr n -> return $ LetExpr n a b
            _ -> failParse "First argument after let not a variable name"

    pairExpr :: Parser Expr
    pairExpr = do
        expr1 <- parseExpr
        symbol "."
        PairExpr expr1 <$> parseExpr

    -- note that this is syntactic sugar, cons is just replaced by the PairExpr ast
    consExpr :: Parser Expr
    consExpr = do
        symbol "cons"
        expr1 <- parseExpr
        PairExpr expr1 <$> parseExpr

    parseParens :: Parser Expr -> Parser Expr
    parseParens p = do
        symbol "("
        e <- p
        symbol ")"
        return e

    -- negate an atom, we actually only have one choice here. Our
    -- parsing already correctly builds negative numbers, and we
    -- can't have negative boolean values (so we won't bother parsing)
    -- those. That leaves variables, but this needs to build a 
    -- NegateExpr around the VarExpr.
    negateAtom :: Parser Expr
    negateAtom = do
        symbol "-"
        a <- parseAtom
        return $ MathExpr Sub [LiteralExpr (IntVal 0), a]

    -- TODO: Implement lambdaExpr 
    -- parse a lambda expression which is a lambda, argument, 
    -- and body, with proper parenthesis around it
    lambdaExpr :: Parser Expr
    lambdaExpr = do
        failParse "Not implemented"

    -- an atom is a literalExpr, which can be an actual literal or some other things
    parseAtom :: Parser Expr
    parseAtom = do
        literalExpr
        <|> varExpr
        <|> negateAtom

    -- the main parsing function which alternates between all the options you have
    parseExpr :: Parser Expr
    parseExpr = do
        parseAtom
        <|> parseParens notExpr
        <|> parseParens boolExpr
        <|> parseParens mathExpr
        <|> parseParens ifExpr
        <|> parseParens applyExpr
        <|> parseParens letExpr
        <|> parseParens lambdaExpr
        <|> parseParens parseExpr
        <|> parseParens compExpr
        <|> parseParens pairExpr
        <|> parseParens consExpr

    -- a helper function that you can use to test your parsing:
    -- syntax is simply 'parseStr "5"' which will call parseExpr for you
    parseStr :: String -> Either ErrorT (Expr, String)
    parseStr str = do parse parseExpr str
