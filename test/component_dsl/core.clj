(ns component-dsl.core
  "Namespace that pretends to create a web server"
  (:require [com.stuartsierra.component :as component]))

(defrecord WebServer [routes port resource-root]
  component/Lifecycle
  (start [this]
    this))

(defn ctor
  [{:keys [port resource-root]
    :or {port 8000
         resource-root "www"}}]
  {:port port
   :resource-root resource-root})
