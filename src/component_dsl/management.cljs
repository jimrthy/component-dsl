(ns component-dsl.management
  (:require [schema.core :as s]))

(s/defn ^:always-validate load-resource :- s/Any
  [url :- s/Str]
  (throw (ex-info "This really should work" {})))
