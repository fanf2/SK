import Text.ParserCombinators.Parsec

infixl 9 :@:

data Expr = Expr :@: Expr | Lambda String Expr | Var String | Num Integer
     deriving (Eq, Read, Show)

tryJoin f p q = p >>= \pp->
               (q >>= \qq-> return (f pp qq))
                        <|> return    pp

expr = lambda <|> tryJoin (:@:) terms (lambda <|> rest)

rest = ch ';' >> expr

lambda = do ch '\\' <|> ch '`'
            var <- name
            lambda_let var <|> lambda_fun var

lambda_let var = do ch '='
                    arg <- terms
                    val <- rest
                    return (Lambda var val :@: arg)

lambda_fun var = do val <- expr
                    return (Lambda var val)

terms = chainl1 term $ return (:@:)

term = brac <|> num <|> var <?> "term"

brac = between (ch '(') (ch ')') expr

num = do digits <- many1 digit
         spret $ Num (read digits)

var = do n <- name
         return $ Var n

name = do a <- letter
          as <- many alphaNum
          spret (a:as)

spret v = spaces >> return v

ch c = char c >> spaces
