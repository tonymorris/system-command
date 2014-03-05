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

import Control.Lens(Prism', Iso', prism', iso, (#), isn't)
import Data.Bool(Bool, not)
import Data.Data(Data, Typeable)
import Data.Function((.))
import Data.Maybe(Maybe(Just, Nothing))
import Data.Semigroup(Semigroup((<>)))
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

