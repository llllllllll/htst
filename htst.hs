import Htst (defaultMain, Job(..), defaults, nosetests)


-- | Zipline job.
zipline :: Job
zipline = defaults
          { jobName        = "zipline"
          , jobDir         = "/home/joejev/quantopian/zipline"
          , jobCmd         = nosetests Nothing
          , jobShouldMove  = \s -> (take 4 $ reverse s) /= "cyp."
          }


main :: IO ()
main = defaultMain [zipline]
