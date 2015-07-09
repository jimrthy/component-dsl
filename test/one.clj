(ns one
  "Basic schema to test loading them"
  (:require [schema.core :as s]))

(def schema-a {:a s/Int, :b s/Int})
(def schema-b {:z s/Str, :y s/Keyword})


