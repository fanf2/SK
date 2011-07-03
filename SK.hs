--
--   Haskell lambda and combinator utilities
--

import Char

infixl 9 :@:

data Expr = Expr :@: Expr | Lambda String Expr | Var String
          | Y | I | J | K | S | C | B | SS | CC | BB
          | P | G | E Int
     deriving (Eq, Read, Show)

data EType = EFun | EArg | ELam deriving Eq


isGraph c =  c >= '!' && c <= '~'
isToken c = isAlphaNum c || '_' == c

disp e = display ELam e ""

exec e = do e' <- execute (compile (fst (parse e)))
            putStr (disp e')

comp = disp . compile  . fst . parse

simp = disp . simplify . fst . parse


-- turn String into Expr

parse (a : as) | isSpace a = parse as
               | a == '`'  = let (var,  bs) = span isToken (dropWhile isSpace as)
                                 (expr, cs) = parse bs
                             in (Lambda var expr, cs)
               | a == '('  = let (left, bs) = parse as
                                 fun left (c : cs) | isSpace c = fun left cs
                                                   | c == ')'  = (left, cs)
                                                   | otherwise = let (right, ds) = parse (c:cs)
                                                                 in fun (left :@: right) ds
                             in fun left bs
               | a == '\'' = case as of
                                  c:cs -> (E (ord c), cs)
                                  []   -> (E (-1), [])
               | isDigit a = let (bs, cs) = span isDigit as
                                 n = read (a:bs)
                             in (E n, cs)
               | a == '-'  = let (bs, cs) = span isDigit as
                                 n = read (if bs == "" then "1" else bs)
                             in (E (-n), cs)
               | isToken a = let (bs, cs) = span isToken as
                             in (case a:bs of
                                "Y" -> Y;  "I" -> I;  "J" -> J;  "K" -> K;  "S" -> S;  "C" -> C;  "B" -> B
                                "SS" -> SS;  "CC" -> CC;  "BB" -> BB;  "P" -> P;  "G" -> G;  x -> Var x
                             , cs)


-- pretty printer

display t (Lambda v e) = showChar '`' . showString v . display ELam e
display t (a :@: b)    = showParen (t /= EFun) (display EFun a . showChar ' ' . display EArg b)
display t (Var v)      = showParen (t == ELam) (showString v)
display t (E n)        = let c = chr n in if isGraph c then showChar '\'' . showChar c else shows n
display t e            = showParen (t == ELam) (shows e)


-- translate into optimised combinators

compile (a :@: b)      = compile a :@: compile b
compile (Lambda var e) = abstract var (compile e)
compile e              = e

abstract v (a :@: b)      = optimise (abstract v a) (abstract v b)
abstract v e | e == Var v = I
             | otherwise  = K :@: e

optimise (K :@: p)              I        =            p
optimise (K :@: p)       (K :@: r)       = K  :@: (  p    :@: r  )
optimise (K :@: p)       (B :@: r :@: s) = BB :@:   p     :@: r :@: s
optimise (K :@: p)              r        = B  :@:  p      :@: r
optimise (B :@: p :@: q) (K :@: r)       = CC :@: p :@: q :@: r
optimise        p        (K :@: r)       = C  :@: p       :@: r
optimise (B :@: p :@: q)        r        = SS :@: p :@: q :@: r
optimise        p               r        = S  :@: p       :@: r


-- do combinator reduction

execute (    Y    :@: f) = let g = f :@: g in execute                       g          
execute (    I    :@: f   )                 = execute         f                        
execute (    J    :@: f :@: g)              = execute                       g          
execute (    K    :@: f :@: g   )           = execute         f                        
execute (    S    :@: f :@: g :@: x)        = execute       ((f :@: x) :@: (g :@: x))  
execute (    C    :@: f :@: g :@: x   )     = execute       ((f :@: x) :@:  g)         
execute (    B    :@: f :@: g :@: x      )  = execute        (f        :@: (g :@: x))  
execute (SS :@: c :@: f :@: g :@: x)        = execute (c :@: (f :@: x) :@: (g :@: x))  
execute (CC :@: c :@: f :@: g :@: x   )     = execute (c :@: (f :@: x) :@:  g)         
execute (BB :@: c :@: f :@: g :@: x      )  = execute (c :@: (f        :@: (g :@: x))) 

execute (G :@: f) = do c <- getChar
                       execute (f :@: E (ord c))

execute (P :@: f :@: g) = do f' <- execute f
                             case f' of
                                  E n -> do putChar (chr n)
                                            execute (I :@: g)
                                  _   -> return (f' :@: g)

execute (E n :@: f) = do f' <- execute f
                         case f' of
                              E m -> if n == m then return K else return J
                              _ -> return (E n :@: f')

execute (f :@: g) = do f' <- execute f
                       if f' == f then return  (f' :@: g)
                                  else execute (f' :@: g)
execute e = return e


-- translate into lambda calculus and do beta reduction

simplify I  = fst (parse "   `f         f            ")
simplify J  = fst (parse "   `f`g             g      ")
simplify K  = fst (parse "   `f`g       f            ")
simplify S  = fst (parse "   `f`g`x   ((f x) (g x))  ")
simplify C  = fst (parse "   `f`g`x   ((f x)  g)     ")
simplify B  = fst (parse "   `f`g`x    (f    (g x))  ")
simplify CC = fst (parse " `c`f`g`x (c (f x)  g)     ")
simplify SS = fst (parse " `c`f`g`x (c (f x) (g x))  ")
simplify BB = fst (parse " `c`f`g`x (c (f    (g x))) ")

simplify (Lambda v b) = Lambda v (simplify b)

simplify (Lambda v b :@: a) = simplify (substitute a v b)

simplify (a :@: b) = let a' = simplify a
                         b' = simplify b
                     in if a' == a then (a' :@: b')
                          else simplify (a' :@: b')
simplify x = x


-- replace instances of variable v in body b with argument a, avoiding variable capture

substitute a v b = let subst (Var v')       | v == v'   = a
                                            | otherwise = Var v'
                       subst (Lambda v' b') | v == v'   = Lambda v' b'
                                            | otherwise = let w = newvar ([v] ++ (vars a) ++ (vars b'))
                                                          in Lambda w (subst (substitute (Var w) v' b'))
                       subst (b :@: c) = (subst b) :@: (subst c)
                       subst x = x
                   in subst b


-- return a variable not in vs (which is sorted)

newvar vs = let try v = if any (v ==) vs then try (next v) else v
                next (v:vs) = if v < 'z' then (succ v):vs else 'a':(next vs)
                next [] = "a"
            in try "a"


-- get (sorted, unrepetitive) list of variables in an Expr

vars (Lambda v b) = [v] ++ (vars b)
vars (a :@: b)    = (vars a) ++ (vars b)
vars (Var v)      = [v]
vars _            = []
