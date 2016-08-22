(ns component-dsl.example-db
  (:require [com.stuartsierra.component :as cpt]))

(defn ctor
  "Pretend to establish a database connection"
  [options]
  (throw (ex-info "write this" {})))

(defn schema-builder
  "Set up the database's schema"
  [options]
  (throw (ex-info "write this" {})))
