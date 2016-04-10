(ns build.steps
  (:require [lambdacd.steps.shell :as shell]))

(defn some-step-that-does-nothing [args ctx]
  {:status :success})

(def path-root "../")

(defn stack-build [args ctx]
  (shell/bash ctx path-root "stack build"))

(defn stack-run [args ctx]
  (shell/bash ctx path-root "stack exec elblog-server"))

(defn stack-test [args ctx] 
  (shell/bash ctx path-root "stack test"))

(defn gulp-build [args ctx]
  (shell/bash ctx path-root "gulp"))

(defn some-failing-step [args ctx]
  (shell/bash ctx "/" "echo \"i am going to fail now...\"" "exit 1"))
