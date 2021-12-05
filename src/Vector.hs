{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Vector where

data Nat = Zero | Succ Nat deriving (Show)

data Vector (n :: Nat) (a :: *) where
  VNil :: Vector 'Zero a
  VCons :: a -> Vector n a -> Vector ('Succ n) a

instance Show a => Show (Vector n a) where
  show VNil = "VNil"
  show (VCons a as) = "VCons[" ++ show a ++ "](" ++ show as ++ ")"

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ n) m = add n (Succ m)

type family Add n m where
  Add 'Zero n = n
  Add ('Succ n) m = 'Succ (Add n m)

append :: Vector n a -> Vector m a -> Vector (Add n m) a
append VNil xs = xs
append (VCons a rest) xs = VCons a (append rest xs)
