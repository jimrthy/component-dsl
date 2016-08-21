(ns one
  "Basic schema to test loading them"
  (:require [clojure.spec :as s]))

(s/def ::a int?)
(s/def ::b int?)
(s/def ::schema-a (s/keys :req [::a ::b]))

(s/def ::y keyword?)
(s/def ::z string?)
(s/def ::schema-b (s/keys :req [::y ::z]))

(def for-checking-var-extraction "from ns one")
(def also "also from ns one")
