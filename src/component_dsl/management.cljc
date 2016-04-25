(ns component-dsl.management
  (:require [clojure.edn :as edn]
            [schema.core :as s]))

(s/defn ^:always-validate load-resource :- s/Any
  [url :- s/Str] ; Actually, this should probably accept a URI
  (-> url
      clojure.java.io/resource
      slurp
      edn/read-string))
