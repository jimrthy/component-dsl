(ns two
  "Some more test schema"
  (:require [clojure.spec :as s]))

(s/def ::a string?)
(s/def ::z int?)

(s/def ::schema-a (s/keys :req [::a ::z]))

(def for-checking-var-extraction "from ns two")
