# `htst` #

A tool for running unit tests and stuff.


## Purpose ##

When running really long unittests for python, I wanted to be able to continue
working; however, `nosetests` does not pre-import all the modules used
throughout the test suite, so I could not change branches and work while the
tests ran. I needed something to copy the code into a clean directory and then
run the tests from there. I also wanted to be able to attach some notification
to the completion of the tests, and possibly work towards parallelization
strategies for the tests themselves.


## `Job` ##

A `Job` is a specification for a unittest suite to run.
You create `Job`s in your `htst.hs` file.

You can configure:

1. `jobName :: String`: The name to use when invoked from the command line.
1. `jobDir :: FilePath`: The root of the project.
1. `jobCmd :: JobID -> IO JobResult`: The function to run to execute the test.
1. `jobShouldMove :: String -> Bool`: The function to filter the contents of the
   project when moving it. For example, to not copy over python bytecode.
1. `jobSuccessHook :: JobID -> IO ()`: The function to run on success.
1. `jobFailureHook :: JobID -> Maybe Int -> IO ()`: The function to run on
   failure. This is the number of tests that failed if they could be decided.


### Defining our own `Job`s ###

To extend `htst` you will want to write your own `Job`s. This means
understanding the `JobResult` and the structure of how `Job`s will be run.

There are two options when you invoke `htst`, `-r` or `-b`. The `-r --run`
command will run the given `jobName` asyncronously and does not block the given
terminal. This will still invoke the proper hooks when needed. the `-b --block`
command will run the `jobName` in the same terminal, blocking the program until
it is finished. More than one of these can be passed in a single command. For
example, if you pass 2 `-r` commands, then both will run at the same time.



#### `jobDir` and `jobShouldMove` ####

The `jobDir` is the full path to your project. This should be the root of the
project, not just the test suite directory. When a `Job` is run, this will be
recursivly copied into a tmp directory where the test will be run. This is
copied to help isolate the testing environment.

The `jobShouldMove` function takes the filepath for a given file and returns
`True` if it should be copied into the tmp file tree. This can be used to not
copy extranous files. This can be used to strip a project of python bytecode
before running the tests to make sure the test suite does not depend on any
stale modules.


#### `jobCmd` and `JobResult` ####

The `jobCmd` is the function that takes our `JobID`, a unique identifier for the
current test run, and executes the test action itself, returning the
`JobResult`. This function can call out to other binaries and parse their
output, or run some haskell function, as it is an `IO` action.

A `JobResult` represents the outcome of a test. It is either a `JobSuccess` or
`JobFailure`. A `JobSuccess` means that the test passed, no extra data is passed
along. a `JobFailure` holds a `Maybe Int` representing the number of tests that
failed in the given suite. This is represented as a `Maybe` because it may not
be known how many tests failed for various reasons, or that might not be a
logical representation for your given test suite.


#### `jobSuccessHook` and `jobFailureHook` ####

The `jobSuccessHook` is the function that will be called when a test
passes. This can be used to alert the user that the test succeeded.

the `jobFailureHook` is called when the test does not pass. This is passed the
`JobID` along with the `Maybe Int` pulled out of the `JobFailure`. This is
supposed to notify the user that the tests failed.

These are both `IO` actions so that they can update the state of the world to
reflect the status of the `Job` that was just run.




## Example `htst.hs` file ##

```haskell
import Htst (defaultMain, Job(..), defaults, parNose)


noByteCode :: FilePath -> Bool
noByteCode = (/=) "cyp." . take 4 . reverse


-- | Zipline job.
zipline :: Job
zipline = defaults
          { jobName        = "zipline"
          , jobDir         = "/home/joejev/quantopian/zipline/"
          , jobCmd         = parNose
          , jobShouldMove  = noByteCode
          }


main :: IO ()
main = defaultMain [zipline]
```


In this example, we will only have 1 `Job` named 'zipline' that we can run. This
will run the unittests for [Zipline](https://github.com/quantopian/zipline), a
free algorithmic trading library for python.
