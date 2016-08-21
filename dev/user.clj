(ns user
  (:require [clojure.tools.namespace.repl :refer (refresh)]))

;; This is an old trick from Pedestal. When system.clj doesn't compile,
;; it can prevent the REPL from starting, which makes debugging very
;; difficult. This extra step ensures the REPL starts, no matter what.

(defn dev
  []
  (require 'dev)
  (in-ns 'dev))

(defn version
  "Sanity check, just to see where this ns came from"
  []
  "com.jimrthy.component.dsl")

;;; Even though this is a library, and there really isn't anything to start
(defn go
  []
  (println "Don't you mean (dev) ?"))

(defn reset
  []
  (println "Yep. You mean (dev)"))
