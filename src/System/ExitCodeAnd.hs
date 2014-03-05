{-# LANGUAGE NoImplicitPrelude #-}

module System.ExitCodeAnd(
  ExitCodeAndT
, IOExitCodeAndT
, ExitCodeAnd
) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Data.Functor.Apply
import Data.Functor.Alt
import Data.Functor.Bind
import Data.Functor.Bind.Trans
import Data.Functor.Identity
import Data.Monoid
import System.ExitCode
import System.IO(IO)
import Prelude

newtype ExitCodeAndT f a =
  ExitCodeAndT (f (ExitCode, a))

type ExitCodeAnd a =
  ExitCodeAndT Identity a

type IOExitCodeAndT a =
  ExitCodeAndT IO a

instance Functor f => Functor (ExitCodeAndT f) where
  fmap f (ExitCodeAndT x) =
    ExitCodeAndT (fmap (\(c, a) -> (c, f a)) x)

instance Apply f => Apply (ExitCodeAndT f) where
  ExitCodeAndT f <.> ExitCodeAndT a =
    ExitCodeAndT (liftF2 (<*>) f a)

instance Applicative f => Applicative (ExitCodeAndT f) where
  pure =
    ExitCodeAndT . pure . pure
  ExitCodeAndT f <*> ExitCodeAndT a =
    ExitCodeAndT (liftA2 (<*>) f a)

instance (Functor f, Monad f) => Alt (ExitCodeAndT f) where
  ExitCodeAndT a <!> ExitCodeAndT b =
    ExitCodeAndT (
      a >>= \(c, a') ->
        if isSuccess c
          then
            return (c, a')
          else
            b
    )

instance Bind f => Bind (ExitCodeAndT f) where
  ExitCodeAndT x >>- f =
    ExitCodeAndT (x >>- \(c, a) -> let ExitCodeAndT r = f a in fmap (\(c', b) -> (c `mappend` c', b)) r)

instance Monad f => Monad (ExitCodeAndT f) where
  ExitCodeAndT x >>= f =
    ExitCodeAndT (x >>= \(c, a) -> let ExitCodeAndT r = f a in liftM (\(c', b) -> (c `mappend` c', b)) r)
  return =
    ExitCodeAndT . return . (,) success

instance MonadPlus f => MonadPlus (ExitCodeAndT f) where
  ExitCodeAndT x `mplus` ExitCodeAndT y =
    ExitCodeAndT (x `mplus` y)
  mzero =
    ExitCodeAndT mzero

instance BindTrans ExitCodeAndT where
  liftB =
    ExitCodeAndT . fmap ((,) success)

instance MonadTrans ExitCodeAndT where
  lift =
    ExitCodeAndT . liftM ((,) success)
