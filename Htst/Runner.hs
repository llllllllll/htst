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

import Htst.Core (Job(..))


tmpDir :: FilePath
tmpDir = "/tmp/htst/"


-- | Moves an absolute filepath under the tmpdir.
underTmp :: FilePath -> FilePath
underTmp p = tmpDir </> drop 1 p


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


runAsyncJob :: Job -> IO ()
runAsyncJob = void . forkIO . runJob


runJob :: Job -> IO ()
runJob j = let d = jobDir j
           in cpR (jobShouldMove j) d
                  >> bracketCWD (underTmp d)
                         (runCmdAndHook j
                          >> removeDirectoryRecursive (underTmp d))

bracketCWD :: FilePath -> IO a -> IO a
bracketCWD p a = getCurrentDirectory
                 >>= \cwd -> setCurrentDirectory p
                 >> a
                 >>= \r -> setCurrentDirectory cwd
                 >> return r


runCmdAndHook :: Job -> IO ()
runCmdAndHook j = jobCmd j
                  >>= \s -> if s == 0
                              then jobSuccessHook j
                              else jobFailureHook j s
