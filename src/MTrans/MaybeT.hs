{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}

module MTrans.MaybeT where

import Data.Functor.Identity

newtype MaybeT m a = MaybeT {runMaybeT :: m (Maybe a)}

instance Functor m => Functor (MaybeT m) where
  fmap :: (a -> b) -> MaybeT m a -> MaybeT m b
  fmap f (MaybeT mma) = MaybeT (fmap (fmap f) mma)

instance Applicative m => Applicative (MaybeT m) where
  pure :: a -> MaybeT m a
  pure a = MaybeT {runMaybeT = pure (Just a)}
  (<*>) :: MaybeT m (a -> b) -> MaybeT m a -> MaybeT m b
  (MaybeT mf) <*> (MaybeT mx) = MaybeT (((<*>) <$> mf) <*> mx)

instance Monad m => Monad (MaybeT m) where
  (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
  (MaybeT ma) >>= f =
    MaybeT $
      ma >>= \case
        Nothing -> pure Nothing
        Just v -> runMaybeT (f v)


