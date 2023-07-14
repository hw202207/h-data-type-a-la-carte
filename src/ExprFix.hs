{-# LANGUAGE TypeOperators, InstanceSigs #-}
module ExprFix where

data Expr f = In (f (Expr f))


data Val e = Val Int
type IntExpr = Expr Val
--   Expr Val = In (Val (Expr Val))
--            = In (Val Int)
--              There is only valid `Val` constructor
--              But why recursive (In (Val (In (Val (Expr Val)))))
--
data Add e = Add e e
type AddExpr = Expr Add
--   Expr Add = In (Add (Expr Add))
--            = In (Add (Expr Add) (Expr Add))

data (f :+: g) e = Inl (f e) | Inr (g e)

addExample :: Expr (Val :+: Add)
addExample = In (Inr (Add (In (Inl (Val 118))) (In (Inl (Val 1219)))))

{- I don't understand this. How does it when to chose Inr v.s. Inl
addExample = In ( (Val :+: Add) (Expr (Val :+: Add)) )
           = In ( ??? )

This is called co-product of Monad.
See more in <Composing Monads Using Coproducts>
-}


instance Functor Val where
  -- TODO: this is the code from paper where `f` is not used at all.
  fmap f (Val x) = Val x

instance Functor Add where
  fmap :: (a -> b) -> Add a -> Add b
  fmap f (Add e1 e2) = Add (f e1) (f e2)

instance (Functor f , Functor g ) => Functor (f :+: g ) where
  fmap :: (Functor f, Functor g) => (a -> b) -> (:+:) f g a -> (:+:) f g b
  fmap f (Inl e1) = Inl (fmap f e1)
  fmap f (Inr e2) = Inr (fmap f e2)


-- TODO: more to come when understand further


{-
This paper says its advantange to <Open Data Types and Open Functions>
Read this first

-}
