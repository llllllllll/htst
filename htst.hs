import Htst (defaultMain, Job(..), defaults, nosetests)


-- | Zipline job.
zipline :: Job
zipline = defaults
          { jobName        = "zipline"
          , jobDir         = "/home/joejev/quantopian/qexec/zipline_repo/"
          , jobCmd         = nosetests $ Just "tests/utils/test_events.py"
          , jobShouldMove  = \s -> (take 4 $ reverse s) /= "cyp."
          }


main :: IO ()
main = defaultMain [zipline]
