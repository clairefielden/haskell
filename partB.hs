module PartB where

--QUESTION 1

data Expr = Val Int | App Op Expr Expr
           deriving Show

data Op = Add | Mul
        deriving Show 

valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Mul _ _ = True

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Mul x y = x * y

eval :: Expr -> Int
eval (Val n) = n
eval (App Add l r) = eval l + eval r
eval (App Mul l r) = eval l * eval r

values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r) = values l ++ values r

--eval (App Add (App Add (Val 8) (Val 10)) (App Mul (Val 2) (Val 10)))
--values (App Add (App Add (Val 8) (Val 10)) (App Mul (Val 2) (Val 5)))

--QUESTION 2

delete :: Int -> [Int] -> [Int]
delete x []           = []
delete x (y:ys)       = if x == y then ys else y : delete x ys

--QUESTION 3

perms :: [Int] -> [[Int]]
perms [] = [[]]
perms xs = do x <- xs
              let l = delete x xs
              ls <- perms l
              return $ x : ls

--QUESTION 4

split :: [a] -> [([a], [a])]
split [] = []
split [_] = []
split (x : xs) = ([x], xs) : [(x : ls, rs) | (ls, rs) <- split xs]

--QUESTION 5

subs :: [a] -> [[a]]
subs [] = [[]]
subs (x : xs) = yss ++ map (x :) yss
                where yss = subs xs

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- [Mul , Add]]

exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs arg = [ e | (ls, rs) <- split arg,
               l <- exprs ls,
               r <- exprs rs,
               e <- combine l r]

printExpressions :: [Int] -> IO()
printExpressions list =  mapM_ print (exprs list)     

--QUESTION 6

choices :: [Int] -> [[Int]]
choices xs = concat (map perms (subs xs))

type Result = (Expr , Int )

append :: Result -> Result -> [Result]
append (l , x) (r , y) = [(App o l r, apply o x y) | o <- [Mul, Add], valid o x y ]

results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n, n) | n > 0]
results ns = [res | (ls, rs) <- split ns,
                  lx <- results ls,
                  ry <- results rs,
                  res <- append lx ry ]

solve :: [Int] -> Int -> [Expr]
solve ns n = [ e | ns <- choices ns, (e,m) <- results ns, m == n]

printSolutions :: [Int] -> Int -> IO()
printSolutions list n =  mapM_ print (solve list n)     