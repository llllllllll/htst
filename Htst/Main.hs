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


import Control.Arrow ((***))
import Data.List (find)
import Data.Maybe (catMaybes, fromMaybe)
import System.Console.GetOpt ( ArgOrder(..), OptDescr(..)
                             , ArgDescr(..), getOpt, usageInfo
                             )
import System.Environment (getArgs, getProgName)

import Htst.Core (Job(..))
import Htst.Runner (runAsyncJob, runJob)



-- Command line flags.
data Flag = Run String
          | Block String
          | Help
          | Version


options :: [OptDescr Flag]
options = [ Option "r" ["run"] (ReqArg Run "job") "Run the given job."
          , Option "b" ["blocking"] (ReqArg Block "blocking-job")
                       "Run the given job and block execution."
          , Option "v" ["version"] (NoArg Version) "Displays the version info"
          , Option "h" ["help"] (NoArg Help) "Displays this dialogue."
          ]


-- | Parses the 'Flag's and returns the runners and Jobs to run.
parseToRun :: [Flag] -> [Job] -> [(Job -> IO (), Job)]
parseToRun fs js = let ns = map jobName js
                       ks = filter (flip elem ns . snd)
                            $ catMaybes $ map (\case
                                                 Run s   -> Just (False, s)
                                                 Block s -> Just (True, s)
                                                 _       -> Nothing) fs
                   in map (resolveRunner *** unsafeResolveJob) ks
  where
      resolveRunner True  = runJob
      resolveRunner False = runAsyncJob
      unsafeResolveJob n  = fromMaybe e $ find ((==) n . jobName) js
      e                   = error "parseToRun: jobName should not be Nothing"


handleOpts :: [Job] -> String -> ([Flag],[String],[String]) -> IO ()
handleOpts _ argv0 ([], _, _)  = putStrLn $ "Usage:" ++ usageInfo "" options
handleOpts js argv0 (fs, _, _) = mapM_ (\(a, b) -> a b) $ parseToRun fs js


defaultMain :: [Job] -> IO ()
defaultMain js = getProgName
                 >>= \p -> getArgs
                 >>= \as -> handleOpts js p (getOpt RequireOrder options as)
