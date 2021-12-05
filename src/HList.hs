{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module HList where

data HList xs where
  HNil :: HList '[]
  (:::) :: a -> HList as -> HList (a ': as)

instance Show (HList '[]) where
  show HNil = "HNil"

instance
  (Show (HList as), Show a) =>
  Show (HList (a ': as))
  where
  show (a ::: rest) = show a ++ " ::: " ++ show rest

infixr 6 :::
