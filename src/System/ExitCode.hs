{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module System.ExitCode {-(
  -- * Data Type
  ExitCode
  -- * ExitCode combinators
, exitCode
, exitCode'
, success
, isSuccess
, isFailure
) -} where

import Control.Applicative(Applicative((<*>), pure))
import Control.Lens(Prism', Iso', prism', iso, (#), isn't)
import Control.Monad(Monad((>>=), return), liftM)
import Control.Monad.Trans.Class(MonadTrans(lift))
import Control.Monad.IO.Class(MonadIO(liftIO))
import Data.Bool(Bool, not)
import Data.Data(Data, Typeable)
import Data.Function((.))
import Data.Functor(Functor(fmap))
import Data.Functor.Apply(Apply((<.>)), liftF2)
import Data.Functor.Alt(Alt((<!>)))
import Data.Functor.Bind(Bind((>>-)))
import Data.Functor.Bind.Trans(BindTrans(liftB))
import Data.Functor.Extend(Extend(duplicated))
import Data.Maybe(Maybe(Just, Nothing))
import Data.Semigroup(Semigroup((<>)))
import System.IO(IO)
import Prelude(Read, Show, Eq((==)), Ord, Int)

-- | The result of running a process
data ExitCode a =
  ExitFailure Int
  | ExitSuccess a
  deriving (Eq, Ord, Show, Read, Data, Typeable)

instance Semigroup (ExitCode a) where
  ExitFailure _ <> x =
    x
  ExitSuccess a <> _ =
    ExitSuccess a

instance Functor ExitCode where
  fmap _ (ExitFailure n) =
    ExitFailure n
  fmap f (ExitSuccess a) =
    ExitSuccess (f a)

instance Apply ExitCode where
  ExitFailure n <.> _ =
    ExitFailure n
  ExitSuccess f <.> r =
    fmap f r

instance Applicative ExitCode where
  (<*>) =
    (<.>)
  pure =
    ExitSuccess

instance Bind ExitCode where
  ExitSuccess a >>- f =
    f a
  ExitFailure n >>- _ =
    ExitFailure n

instance Monad ExitCode where
  ExitSuccess a >>= f =
    f a
  ExitFailure n >>= _ =
    ExitFailure n
  return =
    pure

instance Alt ExitCode where
  ExitFailure _ <!> r =
    r
  ExitSuccess a <!> _ =
    ExitSuccess a

instance Extend ExitCode where
  duplicated (ExitFailure n) =
    ExitFailure n
  duplicated (ExitSuccess a) =
    ExitSuccess (ExitSuccess a)

type ExitCode' =
  ExitCode ()

failure ::
  Prism' Int (ExitCode a)
failure =
  prism'
    (\x -> case x of
             ExitFailure n -> n
             ExitSuccess _ -> 0)
    (\n -> case n of
             0 -> Nothing
             _ -> Just (ExitFailure n))

success ::
  Prism' (ExitCode a) a
success =
  prism'
    ExitSuccess
    (\x -> case x of
             ExitFailure _ -> Nothing
             ExitSuccess a -> Just a)

empty ::
  Iso' Int ExitCode'
empty =
  iso
    (\n -> case n of
             0 -> ExitSuccess ()
             _ -> ExitFailure n)
    (\x -> case x of
             ExitFailure n -> n
             ExitSuccess _ -> 0)

success' ::
  ExitCode'
success' =
  success # ()

isFailure ::
  ExitCode a
  -> Bool
isFailure =
  isn't success

isSuccess ::
  ExitCode a
  -> Bool
isSuccess =
  not . isFailure

newtype ExitCodeT f a =
  ExitCodeT (f (ExitCode a))

type IOExitCode a =
  ExitCodeT IO a

instance Apply f => Semigroup (ExitCodeT f a) where
  ExitCodeT x <> ExitCodeT y =
    ExitCodeT (liftF2 (<>) x y)

instance Functor f => Functor (ExitCodeT f) where
  fmap f (ExitCodeT x) =
    ExitCodeT (fmap (fmap f) x)

instance (Functor f, Monad f) => Apply (ExitCodeT f) where
  ExitCodeT f <.> ExitCodeT a =
    ExitCodeT (f >>= \x -> case x of
                             ExitFailure n -> return (ExitFailure n)
                             ExitSuccess g -> fmap (fmap g) a)

instance (Functor f, Monad f) => Applicative (ExitCodeT f) where
  ExitCodeT f <*> ExitCodeT a =
    ExitCodeT (f >>= \x -> case x of
                             ExitFailure n -> return (ExitFailure n)
                             ExitSuccess g -> fmap (fmap g) a)
  pure =
    ExitCodeT . return . return

instance (Functor f, Monad f) => Bind (ExitCodeT f) where
  ExitCodeT x >>- f =
    ExitCodeT (x >>= \e -> case e of
                             ExitFailure n -> return (ExitFailure n)
                             ExitSuccess a -> let ExitCodeT q = f a in q)

instance (Functor f, Monad f) => Monad (ExitCodeT f) where
  ExitCodeT x >>= f =
    ExitCodeT (x >>= \e -> case e of
                             ExitFailure n -> return (ExitFailure n)
                             ExitSuccess a -> let ExitCodeT q = f a in q)
  return =
    pure

instance Functor f => Extend (ExitCodeT f) where
  duplicated (ExitCodeT x) =
    ExitCodeT (fmap (\e -> case e of
                             ExitFailure n -> ExitFailure n
                             ExitSuccess _ -> ExitSuccess (ExitCodeT x)) x)

instance (Monad f, Functor f) => Alt (ExitCodeT f) where
  ExitCodeT a <!> ExitCodeT b =
    ExitCodeT (a >>= \x -> case x of
                             ExitFailure _ -> b
                             ExitSuccess q -> return (ExitSuccess q))

instance (Functor f, MonadIO f) => MonadIO (ExitCodeT f) where
  liftIO =
    lift . liftIO

instance MonadTrans ExitCodeT where
  lift =
    ExitCodeT . liftM ExitSuccess

instance BindTrans ExitCodeT where
  liftB =
    ExitCodeT . fmap ExitSuccess

-- Iso' ExitCode' E.ExitCode
-- Iso' ExitCode (ExitCodeT Identity)
-- Iso' (ExitCodeT f a) (f (ExitCode a))
-- onSuccess :: f a -> ExitCodeT f b -> f ()
-- onFailure :: f a -> ExitCodeT f b -> f ()

