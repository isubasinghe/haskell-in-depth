{-# LANGUAGE GADTs, KindSignatures, DataKinds, TypeFamilies, UndecidableInstances #-}

module Vector where 

data Nat = Zero | Succ Nat deriving(Show)

data Vector (n :: Nat) (a :: *) where 
  VNil  :: Vector 'Zero a
  VCons :: a -> Vector n a -> Vector ('Succ n) a

instance Show a => Show (Vector n a) where 
  show VNil = "VNil"
  show (VCons a as) = "VCons" ++ show a ++ "(" ++ show as ++ ")"

add :: Nat -> Nat -> Nat 
add Zero n = n
add (Succ n) m = add n (Succ m)

type family Add n m where 
  Add 'Zero n = n
  Add ('Succ n) m = Add n ('Succ m)

