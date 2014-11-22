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
