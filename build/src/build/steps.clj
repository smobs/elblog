(ns build.steps
  (:require [lambdacd.steps.shell :as shell]))

(defn some-step-that-does-nothing [args ctx]
  {:status :success})

(defn stack-build [args ctx]
  (shell/bash ctx "../" "stack build"))

(defn stack-run [args ctx]
  (shell/bash ctx "../" "stack exec elblog-server"))

(defn gulp-build [args ctx]
  (shell/bash ctx "../" "gulp"))

(defn some-failing-step [args ctx]
  (shell/bash ctx "/" "echo \"i am going to fail now...\"" "exit 1"))
