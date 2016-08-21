(ns component-dsl.management
  (:require [clojure.edn :as edn]
            [clojure.spec :as s]))

;; TODO: ^:always-validate
(s/fdef load-resource
        ;; Actually, this should probably accept a URI instance
        :args (s/cat :url string?)
        :ret any?)
(defn load-resource
  [url]
  (-> url
      clojure.java.io/resource
      slurp
      edn/read-string))
