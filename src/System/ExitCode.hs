{-# LANGUAGE DeriveDataTypeable #-}
module System.ExitCode(
  -- * Data Type
  ExitCode
  -- * ExitCode combinators
, exitCode
, exitCode'
, success
, isSuccess
, isFailure
) where

import qualified System.Exit as E(ExitCode(ExitSuccess, ExitFailure))
-- import qualified System.Process as P(system, rawSystem, ProcessHandle, waitForProcess, getProcessExitCode, readProcessWithExitCode, shell, CreateProcess(..), createProcess, readProcess, terminateProcess, runInteractiveCommand, runInteractiveProcess, runProcess, StdStream(..), CmdSpec(..), runCommand, proc)
import Control.Exception(Exception, toException, fromException)
import Control.Lens(Iso', iso, (#), (^.))
import Data.Bool(Bool, not)
import Data.Data(Data, Typeable)
import Data.Function((.))
import Data.Functor(Functor(fmap))
import Data.Monoid(Monoid(mempty, mappend))
import Data.Semigroup(Semigroup((<>)))
import Prelude(Read, Show, Eq((==)), Ord, Int)

-- | The result of running a process
newtype ExitCode =
  ExitCode Int
  deriving (Eq, Ord, Data, Show, Read, Typeable)

instance Exception ExitCode where
  toException =
    toException . (^. exitCode')
  fromException =
    fmap (exitCode' #) . fromException

instance Semigroup ExitCode where
  (<>) =
    mappend

instance Monoid ExitCode where
  mempty =
    success
  a `mappend` b =
    if isSuccess a then b else a

-- | The isomorphism between an @ExitCode@ and an @Int@.
exitCode ::
  Iso' ExitCode Int
exitCode =
  iso
    (\(ExitCode n) -> n)
    ExitCode

-- | The isomorphism to @System.Exit#ExitCode@.
-- /Note: @ExitFailure 0@ is a success./
exitCode' ::
  Iso' ExitCode E.ExitCode
exitCode' =
  iso
    (\(ExitCode n) -> if n == 0 then E.ExitSuccess else E.ExitFailure n)
    (\x -> case x of
             E.ExitSuccess -> success
             E.ExitFailure n -> exitCode # n)


-- | Construct a process result with the value @0@.
success ::
  ExitCode
success =
  exitCode # 0

-- | Returns true if the given process result was constructed with the value @0@, otherwise false.
isSuccess ::
  ExitCode
  -> Bool
isSuccess (ExitCode n) =
  n == 0

-- | Returns false if the given process result was constructed with the value @0@, otherwise true.
isFailure ::
  ExitCode
  -> Bool
isFailure =
  not . isSuccess
