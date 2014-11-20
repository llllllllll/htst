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
