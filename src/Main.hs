{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
module Main where
import Data.Text (Text)
import qualified Data.Text as T

data Expr a where
    Var :: Text -> Expr a
    Lam :: Text -> Expr a -> Expr a
    App :: Expr a -> Expr a -> Expr a

deriving instance Show (Expr a)
deriving instance Eq a => Eq (Expr a)

instance Functor Expr where
    fmap f (Lam v exp) = Lam v (fmap f exp)
    fmap f (App d e) = App (fmap f d) (fmap f e)
    fmap f (Var v) = Var v

subst :: Expr a -> Text -> Expr a -> Expr a
subst (Lam b exp) v arg
    | v == b    = Lam b exp -- Shadowing
    | otherwise = Lam b $ subst exp v arg
subst (App l a) v arg = App (subst l v arg) (subst a v arg)
subst (Var a) v arg
    | a == v = arg
    | otherwise = Var a

eval :: Expr a -> Expr a
eval (App (Lam v exp) arg) = subst (eval exp) v (eval arg)
    where next = eval exp
eval (App f e) = App (eval f) (eval e)
eval (Lam v e) = Lam v $ eval e
eval e = e

main :: IO ()
main = putStrLn . show . eval $ App (Lam "x" (Var "x")) (Var "3")
