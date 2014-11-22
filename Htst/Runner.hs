-- |
-- Module      : Htst.Runner
-- Copyright   : Joe Jevnik
--
-- License     : GPL-2
-- Maintainer  : joejev@gmail.org
-- Stability   : stable
-- Portability : GHC
--
-- Manages running jobs.
{-# LANGUAGE LambdaCase #-}
module Htst.Runner
    ( runJob       -- :: Job -> IO ()
    , runAsyncJob  -- :: Job -> IO ()
    ) where


import Control.Applicative ((<$>))
import Control.Concurrent (forkIO)
import Control.Monad (void)
import System.Directory ( createDirectoryIfMissing
                        , doesDirectoryExist
                        , removeDirectoryRecursive
                        , copyFile
                        , getCurrentDirectory
                        , setCurrentDirectory
                        )
import System.FilePath.Find ( find, always, filePath
                            , (||?), fileType, FileType(..)
                            )
import System.FilePath.Posix ((</>))

import Htst.Core (Job(..), JobResult(..), JobID(..), newJobID)


tmpDir :: FilePath
tmpDir = "/tmp/htst/"


-- | Moves an absolute filepath under the tmpdir.
underTmp :: FilePath -> FilePath
underTmp p = tmpDir </> drop 1 p  -- drop the root '/'


-- | Recursive copy of a directory into the tmpDir.
-- Only copies the files that match the predicate.
cpR :: (String -> Bool) -> FilePath -> IO ()
cpR pred d = find always applyPred d >>= mapM_ cp
  where
      cp p = doesDirectoryExist p
             >>= \b -> (if b
                          then createDirectoryIfMissing True
                          else copyFile p) $ underTmp p
      applyPred = pred <$> filePath ||? (== Directory) <$> fileType


-- | Runs an asynchronous 'Job'
runAsyncJob :: JobID -> Job -> IO ()
runAsyncJob jid j = void . forkIO $ runJob jid j


-- | Executes a 'Job'. This moves the jobDir based on the moving rules
-- in the 'Job', executes the command, and then passes the result to the
-- proper hook.
runJob :: JobID -> Job -> IO ()
runJob jid j = let d = jobDir j
               in cpR (jobShouldMove j) d
                      >> bracketCWD (underTmp d)
                             (runCmdAndHook jid j
                              >> removeDirectoryRecursive (underTmp d))


-- | cd into p, then execute a, then cd back into the cwd.
bracketCWD :: FilePath -> IO a -> IO a
bracketCWD p a = getCurrentDirectory
                 >>= \cwd -> setCurrentDirectory p
                 >> a
                 >>= \r -> setCurrentDirectory cwd
                 >> return r


-- | Execute the jobCmd and then call the proper hook.
runCmdAndHook :: JobID -> Job -> IO ()
runCmdAndHook jid j = jobCmd j jid
                      >>= \case
                            JobFailure rs -> jobFailureHook j jid rs
                            JobSuccess    -> jobSuccessHook j jid
