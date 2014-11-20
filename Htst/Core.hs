-- |
-- Module      : Htst.Core
-- Copyright   : Joe Jevnik
--
-- License     : GPL-2
-- Maintainer  : joejev@gmail.org
-- Stability   : stable
-- Portability : GHC
--
-- Core data.
module Htst.Core
    ( Job(..)
    , defaults
    ) where


-- | A job to run.
data Job = Job { jobName        :: String
               , jobDir         :: Maybe FilePath
               , jobCmd         :: IO Int
               , jobShouldMove  :: String -> Bool
               , jobSuccessHook :: IO ()
               , jobFailureHook :: Int -> IO ()
               }


-- | The default settings for a 'Job'.
defaults :: Job
defaults = Job { jobName        = "default"
               , jobDir         = Nothing
               , jobCmd         = return 1
               , jobShouldMove  = const True
               , jobSuccessHook = print "success"
               , jobFailureHook = const (print "failure")
               }
