(ns component-dsl.example-db
  (:require [com.stuartsierra.component :as cpt]))

(defn ctor
  "Pretend to establish a database connection"
  [options]
  options)

(defn schema-builder
  "Set up the database's schema"
  [options]
  (into {:db nil} options))
