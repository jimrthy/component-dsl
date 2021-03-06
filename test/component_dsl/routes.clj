(ns component-dsl.routes
  "Namespace that pretends to set up URL routing"
  (:require [com.stuartsierra.component :as component]))

(defrecord Routing [handler]
  component/Lifecycle
  (start [this]
    (assoc this handler "A Ring handler"))
  (stop [this]
    (assoc this handler nil)))

(defn ctor [options]
  (->Routing options))
