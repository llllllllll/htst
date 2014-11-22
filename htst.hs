import Htst (defaultMain, Job(..), defaults, parNose)


-- | Zipline job.
zipline :: Job
zipline = defaults
          { jobName        = "zipline"
          , jobDir         = "/home/joejev/quantopian/qexec/zipline_repo"
          , jobCmd         = parNose
          , jobShouldMove  = \s -> (take 4 $ reverse s) /= "cyp."
          }


main :: IO ()
main = defaultMain [zipline]
