# `htst` #

A tool for running unit tests and stuff.


## Purpose ##

When running really long unittests for python, I wanted to be able to continue
working. I needed something to copy the code into a clean directory and then run
the tests from there. I also wanted to be able to attach some notification to
the completion of the tests.


## `Job` ##

You create `Job`s in your `htst.hs` file.

A `Job` is a specification for something to run.

You can configure:

1. `jobName :: String`: The name to use when looking it up.
1. `jobDir :: Maybe FilePath`: The place to move the test code from. If this is
   `Nothing`, the test is done in place.
1. `jobCmd :: IO Int`: The function to run to execute the test.
1. `jobShouldMove :: String -> Bool`: The function to filter the contents of the
   project.
1. `jobSuccessHook :: IO ()`: The function to run on success.
1. `jobFailureHook :: Int -> IO ()`: The function to run on failure. This is
   passed the error code.


## Example `htst.hs` file ##

```haskell
import Htst (defaultMain, runShell, Job(..), runShell)


-- | My jobs
jobs :: [Job]
jobs = [ Job { jobName        = "zipline"
             , jobDir         = Just "/home/joejev/quantopian/zipline"
             , jobCmd         = runShell "nosetests"
             , jobShouldMove  = \s -> (take 3 $ reverse s) == "yp."
             , jobSuccessHook = print "success!"
             , jobFailureHook = const $ print "failure!"
             }
       ]


main :: IO ()
main = defaultMain jobs
```


Here we create a job named `zipline` that will run the zipline tests, only
copying over the `*.py` files.
