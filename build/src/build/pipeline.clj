(ns build.pipeline
  (:use [lambdacd.steps.control-flow]
        [build.steps])
  (:require
        [lambdacd.steps.manualtrigger :as manualtrigger]))

(def pipeline-def
  `(
    manualtrigger/wait-for-manual-trigger
    (in-parallel
     (alias "stack" (run 
                    stack-build
                    stack-test))
      gulp-build)
    stack-run))
