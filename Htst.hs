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
    , runShell     -- :: String -> IO Int
    ) where


import System.Exit (ExitCode(..))
import System.Process (system)

import Htst.Core (Job(..))
import Htst.Main (defaultMain)


runShell :: String -> IO Int
runShell c = system c >>= \case
                            ExitSuccess   -> return 0
                            ExitFailure n -> return n
