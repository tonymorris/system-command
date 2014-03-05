{-# LANGUAGE NoImplicitPrelude #-}

module System.Command
(
  -- * Running sub-processes
  P.createProcess
, P.shell
, P.proc
, P.CreateProcess(..)
, P.CmdSpec(..)
, P.StdStream(..)
, P.ProcessHandle
  -- * Specific variants of createProcess
, readProcessWithExitCode
, P.runCommand
, P.runProcess
, P.runInteractiveCommand
, P.runInteractiveProcess
, P.readProcess
, system
, rawSystem
  -- * Process completion
, exitWith
, exitFailure
, exitSuccess
, waitForProcess
, getProcessExitCode
, P.terminateProcess
, module EC
) where

import qualified System.Exit as E(exitWith)
import qualified System.Process as P(system, rawSystem, ProcessHandle, waitForProcess, getProcessExitCode, readProcessWithExitCode, shell, CreateProcess(..), createProcess, readProcess, terminateProcess, runInteractiveCommand, runInteractiveProcess, runProcess, StdStream(..), CmdSpec(..), runCommand, proc)
import Control.Lens((#), (^.))
import Data.Function((.))
import Data.Functor(Functor(fmap))
import Data.Maybe(Maybe)
import Data.String(String)
import System.IO(IO)
import System.FilePath(FilePath)
import Prelude(Read, Show, Eq((==)), Ord, Int)
import System.ExitCode as EC

-- | Computation 'exitWith' @code@ throws 'ExitCode' @code@.
-- Normally this terminates the program, returning @code@ to the
-- program's caller.  Before the program terminates, any open or
-- semi-closed handles are first closed.
--
-- A program that fails in any other way is treated as if it had
-- called 'exitFailure'.
-- A program that terminates successfully without calling 'exitWith'
-- explicitly is treated as it it had called 'exitWith' 'ExitSuccess'.
--
-- As an 'ExitCode' is not an 'IOError', 'exitWith' bypasses
-- the error handling in the 'IO' monad and cannot be intercepted by
-- 'catch' from the "Prelude".  However it is a 'SomeException', and can
-- be caught using the functions of "Control.Exception".  This means
-- that cleanup computations added with 'Control.Exception.bracket'
-- (from "Control.Exception") are also executed properly on 'exitWith'.
--
-- Note: in GHC, 'exitWith' should be called from the main program
-- thread in order to exit the process.  When called from another
-- thread, 'exitWith' will throw an 'ExitException' as normal, but the
-- exception will not cause the process itself to exit.
exitWith ::
  ExitCode
  -> IO a
exitWith =
  E.exitWith . (^. exitCode')

-- | The computation 'exitFailure' is equivalent to
-- 'exitWith' @(@'exitCode exitfail'@)@,
-- where /exitfail/ is implementation-dependent.
exitFailure ::
  IO a
exitFailure =
  exitWith (exitCode # 1)

-- | The computation 'exitSuccess' is equivalent to
-- 'exitWith' 'success', It terminates the program
-- sucessfully.
exitSuccess ::
  IO a
exitSuccess =
  exitWith success

-- | readProcessWithExitCode creates an external process, reads its
-- standard output and standard error strictly, waits until the process
-- terminates, and then returns the 'ExitCode' of the process,
-- the standard output, and the standard error.
--
-- 'readProcess' and 'readProcessWithExitCode' are fairly simple wrappers
-- around 'createProcess'.  Constructing variants of these functions is
-- quite easy: follow the link to the source code to see how
-- 'readProcess' is implemented.
readProcessWithExitCode ::
  FilePath
  -> [String]
  -> String
  -> IO (ExitCode, String, String)
readProcessWithExitCode p args i =
  fmap (\(e, t, u) -> (exitCode' # e, t, u)) (P.readProcessWithExitCode p args i)

-- | Computation @system cmd@ returns the exit code produced when the
-- operating system runs the shell command @cmd@.
--
-- This computation may fail with
--
--    * @PermissionDenied@: The process has insufficient privileges to
--      perform the operation.
--
--    * @ResourceExhausted@: Insufficient resources are available to
--      perform the operation.
--
--    * @UnsupportedOperation@: The implementation does not support
--      system calls.
--
-- On Windows, 'system' passes the command to the Windows command
-- interpreter (@CMD.EXE@ or @COMMAND.COM@), hence Unixy shell tricks
-- will not work.
system ::
  String
  -> IO ExitCode
system =
  fmap (exitCode' #) . P.system

-- | The computation @'rawSystem' cmd args@ runs the operating system command
-- @cmd@ in such a way that it receives as arguments the @args@ strings
-- exactly as given, with no funny escaping or shell meta-syntax expansion.
-- It will therefore behave more portably between operating systems than 'system'.
--
-- The return codes and possible failures are the same as for 'system'.
rawSystem ::
  String
  -> [String]
  -> IO ExitCode
rawSystem z =
  fmap (exitCode' #) . P.rawSystem z

-- | Waits for the specified process to terminate, and returns its exit code.

-- GHC Note: in order to call @waitForProcess@ without blocking all the
-- other threads in the system, you must compile the program with
-- @-threaded@.
waitForProcess ::
  P.ProcessHandle
  -> IO ExitCode
waitForProcess =
  fmap (exitCode' #) . P.waitForProcess


-- | This is a non-blocking version of 'waitForProcess'.  If the process is
-- still running, 'Nothing' is returned.  If the process has exited, then
-- @'Just' e@ is returned where @e@ is the exit code of the process.
getProcessExitCode ::
  P.ProcessHandle
  -> IO (Maybe ExitCode)
getProcessExitCode =
  (fmap . fmap) (exitCode' #) . P.getProcessExitCode
