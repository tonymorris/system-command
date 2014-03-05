{-# LANGUAGE NoImplicitPrelude #-}

module System.ExitCodeAnd(
  ExitCodeAndT
, IOExitCodeAndT
, ExitCodeAnd
) where

import Control.Applicative(Applicative((<*>), pure), liftA2)
import Control.Monad
import Control.Monad.IO.Class(MonadIO(liftIO))
import Control.Monad.Trans.Class(MonadTrans(lift))
import Data.Function((.))
import Data.Functor.Apply(Apply((<.>)))
import Data.Functor.Alt(Alt((<!>)), liftF2)
import Data.Functor.Bind(Bind((>>-)))
import Data.Functor.Bind.Trans(BindTrans(liftB))
import Data.Functor.Identity(Identity)
import Data.Monoid(mappend)
import System.ExitCode(ExitCode, isSuccess, success)
import System.IO(IO)

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

instance MonadIO f => MonadIO (ExitCodeAndT f) where
  liftIO =
    lift . liftIO
