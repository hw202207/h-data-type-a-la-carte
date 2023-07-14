module Expr where


data Expr = Val Int | Add Expr Expr

eval :: Expr -> Int
eval (Val x) = x
eval (Add a b) = eval a + eval b

render :: Expr -> String
render (Val x) = show x
render (Add a b) = "(" ++ render a ++ " + " ++ render b ++ ")"

test1 :: Expr
test1 = Add (Val 3) (Val 5)

main :: IO ()
main = do
  putStrLn (render test1)
  print (eval test1)
