{-# LANGUAGE NoImplicitPrelude #-}

module System.ExitCodeAnd(
  ExitCodeAndT
, ExitCodeT
, IOExitCodeAndT
, ExitCodeAnd
, ExitCode'
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
import Data.Semigroup(Semigroup((<>)))
import Data.Monoid(Monoid(mappend, mempty))
import System.ExitCode(ExitCode, isSuccess, success)
import System.IO(IO)

newtype ExitCodeAndT f a =
  ExitCodeAndT (f (ExitCode, a))

type ExitCodeAnd a =
  ExitCodeAndT Identity a

type ExitCodeT f =
  ExitCodeAndT f ()

type ExitCode' =
  ExitCodeAnd ()

type IOExitCodeAndT a =
  ExitCodeAndT IO a

-- Iso' (ExitCodeAndT f a) (f (ExitCode, a))
-- Iso' (ExitCodeAnd a) (ExitCode, a)
-- Iso' (ExitCodeT f) (f ExitCode)
-- Iso' ExitCode' ExitCode
-- Lens (ExitCodeAnd a) (ExitCodeAnd b) a b
-- Lens' (ExitCodeAnd a) ExitCode
-- Functor f => ExitCodeAndT f a -> ExitCodeAndT f ExitCode
-- exitSuccessAnd :: Functor f => f a -> ExitCodeAndT f a
-- exitFailureAnd :: Functor f => Int -> f a -> ExitCodeAndT f a
-- liftExitCodeAnd :: Applicative f => ExitCodeAnd a -> ExitCodeAndT f a
-- tell :: Functor f => f ExitCode -> ExitCodeAndT f ()

instance Monad f => Semigroup (ExitCodeAndT f a) where
  ExitCodeAndT a <> ExitCodeAndT b =
    ExitCodeAndT (
      a >>= \(c, a') ->
        if isSuccess c
          then
            return (c, a')
          else
            b
    )

instance (Monoid a, Monad f) => Monoid (ExitCodeAndT f a) where
  mappend =
    (<>)
  mempty =
    ExitCodeAndT (return (success, mempty))

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
  (<!>) =
    (<>)

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
