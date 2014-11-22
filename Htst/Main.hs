-- |
-- Module      : Htst.Main
-- Copyright   : Joe Jevnik
--
-- License     : GPL-2
-- Maintainer  : joejev@gmail.org
-- Stability   : stable
-- Portability : GHC
--
-- main definition.
{-# LANGUAGE LambdaCase #-}
module Htst.Main
    ( defaultMain  -- :: [Job] -> IO ()
    ) where


import Control.Arrow (second)
import Control.Monad (when)
import Data.List (find)
import Data.Maybe (catMaybes, fromMaybe)
import System.Console.GetOpt ( ArgOrder(..), OptDescr(..)
                             , ArgDescr(..), getOpt, usageInfo
                             )
import System.Environment (getArgs, getProgName)

import Htst.Core (Job(..), newJobID)
import Htst.Runner (runAsyncJob, runJob)


versionStr :: String
versionStr = "htst 0.1.0.0"


-- Command line flags.
data Flag = Run String
          | Block String
          | Help
          | Version
            deriving (Eq)


options :: [OptDescr Flag]
options = [ Option "r" ["run"] (ReqArg Run "job") "Run the given job."
          , Option "b" ["blocking"] (ReqArg Block "blocking-job")
                       "Run the given job and block execution."
          , Option "v" ["version"] (NoArg Version) "Displays the version info"
          , Option "h" ["help"] (NoArg Help) "Displays this dialogue."
          ]


-- | Parses the 'Flag's and returns IO actions representing the jobs to run.
parseToRun :: [Flag] -> [Job] -> [IO ()]
parseToRun fs js = map ffunc $ catMaybes $ map (\case
                                                  Run s   -> Just (async, s)
                                                  Block s -> Just (blocking, s)
                                                  _       -> Nothing) fs
  where
      ns                  = map jobName js
      ffunc (a, b)        = if b `elem` ns
                              then a $ unsafeResolveJob b
                              else putStrLn $ b ++ " is not a known job"
      unsafeResolveJob n  = fromMaybe e $ find ((==) n . jobName) js
      e                   = error "parseToRun: jobName should not be Nothing"
      newIDPrint          = newJobID
                            >>= \jid -> (putStrLn . show) jid
                            >> return jid
      async    j          = newIDPrint >>= \jid -> runAsyncJob jid j
      blocking j          = newIDPrint >>= \jid -> runJob      jid j



-- | Parse the command line args based on the jobs provided.
handleOpts :: [Job] -> String -> ([Flag],[String],[String]) -> IO ()
handleOpts _ argv0 ([], _, _)  = putStrLn $ "Usage:" ++ usageInfo "" options
handleOpts js argv0 (fs, _, _) = doHelp fs
                                 >> doVersion fs
                                 >> mapM_ id (parseToRun fs js)


-- | Print the help info if the 'Help' flag was passed.
doHelp :: [Flag] -> IO ()
doHelp fs = when (Help `elem` fs) $ putStrLn $ "Usage:" ++ usageInfo "" options


-- | Print the version info if the 'Version' flag was passed.
doVersion :: [Flag] -> IO ()
doVersion fs = when (Version `elem` fs) $ putStrLn $ versionStr


-- | The default main for htst. This provides the argparsing and
-- spins of jobs as needed.
defaultMain :: [Job] -> IO ()
defaultMain js = getProgName
                 >>= \p -> getArgs
                 >>= \as -> handleOpts js p (getOpt RequireOrder options as)
