{-# LANGUAGE GADTs, KindSignatures, DataKinds #-}

module Vector where 

data Nat = Zero | Succ Nat deriving(Show)

data Vector (n :: Nat) (a :: *) where 
  VNil  :: Vector 'Zero a
  VCons :: a -> Vector n a -> Vector ('Succ n) a

instance Show a => Show (Vector n a) where 
  show VNil = "VNil"
  show (VCons a as) = "VCons" ++ show a ++ "(" ++ show as ++ ")"
