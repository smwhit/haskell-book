data Expr
    = Lit Integer
    | Add Expr Expr
    deriving Show

eval :: Expr -> Integer
eval (Lit x) = x
eval (Add x y) = eval x + eval y

printExpr :: Expr -> String
printExpr (Lit x) = show x
printExpr (Add x y) = printExpr x ++ " + " ++ printExpr y

a = printExpr (Add (Lit 1) (Lit 9001))
b = Add (Lit 9001) (Lit 1)
c = Add b (Lit 20001)
d = Add (Lit 1) c