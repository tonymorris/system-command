{-# LANGUAGE NoImplicitPrelude #-}

module System.Command.Monad
(
  Command(runCommand)
, command
) where

import Control.Applicative(Applicative((<*>), pure), Alternative((<|>), empty), liftA2)
import Control.Monad(Monad(return, (>>=)), MonadPlus(mplus, mzero), liftM)
import Control.Monad.Trans.Class(MonadTrans(lift))
import Control.Monad.IO.Class(MonadIO(liftIO))
import Data.Either(Either(Left, Right), either)
import Data.Function((.), ($))
import Data.Functor(Functor(fmap))
import Data.String(String)
import Data.Monoid(Monoid(mempty))
import System.Command(ExitCode, readProcessWithExitCode, isSuccess)
import System.FilePath(FilePath)

newtype Command m a =
  Command { runCommand :: m (Either (ExitCode, String) a) }

command ::
  MonadIO m
  => FilePath
  -> [String]
  -> String
  -> Command m String
command prog args stdin =
  Command $ do
    (code, stdout, stderr) <- liftIO $ readProcessWithExitCode prog args stdin
    return $ if isSuccess code then Right stdout else Left (code, stderr)

instance Functor m => Functor (Command m) where
  fmap f (Command m) =
    Command (fmap (either Left (Right . f)) m)

instance Applicative m => Applicative (Command m) where
  pure =
    Command . pure . pure
  Command mf <*> Command mx =
    Command (liftA2 (<*>) mf mx)

instance Alternative m => Alternative (Command m) where
  empty =
    Command (pure (Left mempty))
  Command mx <|> Command my =
    Command (mx <|> my)

instance Monad m => Monad (Command m) where
  return =
    Command . return . Right
  Command mx >>= f =
    Command (mx >>= either (return . Left) (runCommand . f))

instance MonadPlus m => MonadPlus (Command m) where
  mzero =
    Command (return (Left mempty))
  Command mx `mplus` Command my =
    Command (mx `mplus` my)

instance MonadTrans Command where
  lift =
    Command . liftM Right

instance MonadIO m => MonadIO (Command m) where
  liftIO =
    lift . liftIO
