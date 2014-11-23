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
    , JobResult(..)
    , JobID(..)
    , newJobID  -- :: IO JobID
    , defaults
    ) where


import Control.Applicative ((<$>))
import Control.Monad (liftM2)
import Data.Monoid (Monoid(..))
import Data.UUID (UUID, toString)
import Data.UUID.V4 (nextRandom)


-- | The results returned from running a job.
data JobResult = JobSuccess
               | JobFailure (Maybe Int)
                 deriving (Show)


-- | 'Monoid' instance where failures sum the test failure count,
-- and failures replace successes. An empty 'JobResult' has no failures
-- so it is a 'JobSuccess'.
instance Monoid JobResult where
    mempty                                = JobSuccess
    mappend JobSuccess JobSuccess         = JobSuccess
    mappend JobSuccess (JobFailure n)     = JobFailure n
    mappend (JobFailure n) JobSuccess     = JobFailure n
    mappend (JobFailure n) (JobFailure m) = JobFailure (liftM2 (+) n m)


-- | Each job is run with a unique identifier. This is an ADT so that we can
-- change the underlying representation later if we choose. Nothing should
-- depend on this being a wrapper around UUID.
newtype JobID = JobID UUID


instance Show JobID where
    show (JobID n) = toString n


-- | Generate a new 'JobID' to represent a test run.
newJobID :: IO JobID
newJobID = JobID <$> nextRandom


-- | A job to run.
data Job = Job { jobName        :: String
               , jobDir         :: FilePath
               , jobCmd         :: JobID -> IO JobResult
               , jobShouldMove  :: FilePath -> Bool
               , jobSuccessHook :: JobID -> IO ()
               , jobFailureHook :: JobID -> Maybe Int -> IO ()
               }


-- | The default settings for a 'Job'.
defaults :: Job
defaults = Job { jobName        = "default"
               , jobDir         = "/dev/null"
               , jobCmd         = const . return $ JobFailure Nothing
               , jobShouldMove  = const True
               , jobSuccessHook = \jid -> putStrLn
                                  $ "Job " ++ show jid ++ " success"
               , jobFailureHook = \jid r -> putStrLn
                                  $ "Job " ++ show jid ++ " failed"
                                  ++ (case r of
                                          Nothing -> ""
                                          Just n  -> ' ' : show n ++ " tests")
               }
