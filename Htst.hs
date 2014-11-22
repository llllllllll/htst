-- |
-- Module      : Htst
-- Copyright   : Joe Jevnik
--
-- License     : GPL-2
-- Maintainer  : joejev@gmail.org
-- Stability   : stable
-- Portability : GHC
--
-- API entry point.
{-# LANGUAGE LambdaCase #-}
module Htst
    ( defaultMain  -- :: [Job] -> IO ()
    , Job(..)
    , nosetests    -- :: Maybe FilePath -> JobID -> IO JobResult
    , parNose      -- :: JobID -> IO JobResult
    , defaults     -- :: Job
    ) where


import Control.Applicative ((<$>))
import Control.Concurrent (runInBoundThread)
import Control.Monad (liftM)
import Data.List (lines)
import Data.Maybe (catMaybes)
import Data.Monoid (mconcat)
import System.Exit (ExitCode(..))
import System.Directory (getCurrentDirectory, findExecutable)
import System.FilePath.Find ( find, fileName, extension, always
                            , (&&?), fileType, FileType(..)
                            )
import System.Process (readProcessWithExitCode)
import Text.Read (readMaybe)

import Htst.Core (Job(..), JobID(..), JobResult(..), defaults)
import Htst.Main (defaultMain)


-- | Runs the python testing program "nostests".
-- If FP is pathed, it will passed as the first argument to nosetests.
nosetests :: Maybe FilePath -> JobID -> IO JobResult
nosetests fp _ = findExecutable "nosetests"
                 >>= \case
                       Nothing -> return $ JobFailure Nothing
                       Just n  -> readProcessWithExitCode n (catMaybes [fp]) ""
                                  >>= \case
                                          (ExitSuccess, _, _)     ->
                                              return JobSuccess
                                          (ExitFailure _, _, err) ->
                                              return $ readFailure
                                                         . reverse . lines $ err
  where
      readFailure []    = JobFailure Nothing
      readFailure (c:_) = JobFailure
                          $ readMaybe (drop 17 . reverse . drop 1 . reverse $ c)


-- | Parallel nosetests
parNose :: JobID -> IO JobResult
parNose jid = getCurrentDirectory
              >>= find always testFls
              >>= mapM (\f -> runInBoundThread $ nosetests (Just f) jid)
              >>= return . mconcat
  where
      testFls  = ((==) RegularFile) <$> fileType
                 &&? ((==) "test" . take 4) <$> fileName
                 &&? ((==) ".py") <$> extension
